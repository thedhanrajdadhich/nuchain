# Ansible and AWS

## QuickStart
1. Spin up an EC2 instance with BlockOn's ScalableBFT AMI or with the desired configurations
   (See [Instance Requirements](#instance-requirements)). This will serve as the Ansible monitor instance.
2. Ensure that the key pair(s) of the monitor and Nuchain server instances are not publicly
   viewable: `chmod 400 /path/to/keypair.pem`. Otherwise, SSH and any service that rely on it (i.e. Ansible)
   will not work.
3. Add the key pair(s) of the monitor and Nuchain server instances to the `ssh-agent`:
   `ssh-add /path/to/keypair.pem`
4. SSH into the monitor instance using ssh-agent forwarding: `ssh -A <instance-user>@<instance-public-dns>`.
   This facilitates the Ansible monitor's task of managing different instances by having access to their key pair.
5. Once logged into the monitor instance, locate the directories containing the Nuchain executables,
   the Nuchain server node configurations, and the Ansible playbooks.
6. Edit the `ansible_vars.yml` to indicate the path to the Nuchain executables and the node configurations.
   Also indicate the number of EC2 instances designated as Nuchain servers to launch as well as how to configure
   them. See [Instance Requirements](#instance-requirements) and [Security Group Requirements](#security-group-requirements) for instance image
   and security group specifics.
7. Grant Ansible the ability to make API calls to AWS on your behalf. To do this, launch the monitor instance with
   Power User IAM role or export AWS security credentials as environment variables:
   ```
   $ export AWS_ACCESS_KEY_ID='AK123'
   $ export AWS_SECRET_ACCESS_KEY='abc123'
   ```
   Make sure to persist these environment variables when logging in and out of the monitor instance.

You are now ready to start using the Ansible playbooks!

## Ansible Playbooks
Playbooks are composed of `plays`, which are then composed of `tasks`. Plays
and tasks are executed sequentially.

Ansible playbooks are in YAML format and can be executed as follows:
`ansible-playbook /path/to/playbook.yml`. The `aws/` directory contains the
following playbooks:

`start_instances.yml` : This playbook launches EC2 instances that have the
                        necessary files and directories to run the Nuchain
                        Server executable. It also creates a file containing
                        all of their private IP addresses and the default
                        (i.e. SQLite backend) node configurations for each.
                        This will create instances tagged as "nuchain_server".
                        This list of IP addresses will be located in
                        `aws/ipAddr.yml`.

`stop_instances.yml` : This playbook terminates all Nuchain Server EC2
                       instances.

`run_servers.yml` : This playbooks runs the Nuchain Server executable. If the
                    servers were already running, it terminates them as well
                    as cleans up their sqlite and log files before launching
                    the server again. This playbook also updates the server's
                    configuration if it has changed in the specified
                    configuration directory (conf/) on the monitor instance.
                    The Nuchain Servers will run for 24 hours after starting.
                    To change this, edit the `Start Nuchain Servers` async
                    section in this playbook.

`get_server_logs.yml` : This playbook retrieves all of the Nuchain Servers' logs
                        and sqlite files, deleting all previous retrieved logs.
                        It stores the logs in `aws/logs/`.

NB: To change distributed nodes' configuration, run
```
<nuchain-directory>$ ./bin/<OS-name>/genconfs --distributed aws/ipAddr.yml
```
Provide the desired settings when prompted. For more information, refer to the
"Automated configuration generation: `genconfs`" section in `docs/Nuchain-README.md`.

## Launching the Demo
The demo script assumes that the `start_instances.yml` playbook has been run and only
four Nuchain Server instances have been created. It also assumes the following directory structure:
```
$ tree <nuchain-directory>
<nuchain-directory>
├── aws
    ├── ansible_vars.yml
    ├── get_server_logs.yml
    ├── ipAddr.yml		(produced by start_instances.yml)
    ├── run_servers.yml
    ├── start_aws_demo.sh
    ├── start_instances.yml
    ├── stop_instances.yml
    └── templates
	└── ipAddr.j2
└── bin
    └── <OS-name>
        └── <all nuchain executables>
```

Navigate to `/path/to/nuchain-directory` and run the following commands:
```
tmux
aws/start_aws_demo.sh
```
Press ENTER to run the commands that populates the shell. This will start the Nuchain Client.
See "Sample Usage: `[payments|monitor|todomvc]`" in `Nuchain-README.md` for a list of supported interactions.

To exit the Nuchain Client, type `exit`. To kill the tmux sessions, type `tmux kill-session`.

## Instance Requirements
The Ansible monitor instance and the Nuchain server instances should be configured as follows:
1. Install all Nuchain software requirements. Refer to `<nuchain-directory>/docs/Nuchain-README.md` for specifics.
2. Have Ansible 2.6+ installed.
   See <https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html> for instructions.
3. Setup Ansible to use EC2's external inventory script.
   See <https://docs.ansible.com/ansible/latest/user_guide/intro_dynamic_inventory.html#example-aws-ec2-external-inventory-script> for instructions.

An AWS image (AMI) created from this configured instance could be used to launch the Ansible monitor and Nuchain server
instances. For more information, see <https://docs.aws.amazon.com/toolkit-for-visual-studio/latest/user-guide/tkv-create-ami-from-instance.html>.

See `setup/setup-ubuntu-base.sh` for an example on how to configure EC2's free-tier ubuntu machine to run
the Nuchain executables and Ansible.


## Security Group Requirements
Ansible needs to be able to communicate with the AWS instances it manages, and the Nuchain Servers need to communicate
with each other. Therefore, the security group (firewall) assigned to the Nuchain server instances
should allow for the following:
1. The Ansible monitor instance (the one running the playbooks) should be able to ssh into
   all of the Nuchain Server instances it will manage.
2. The Nuchain Server instances should be able to communicate via TCP 10000 port.
3. The Nuchain Server instances should be able to receive HTTP connections via the 8000 port from
   any instance running the Nuchain Client.

The simplest solution is to create a security group that allows all traffic among itself and assign this security
group to the Ansible monitor and Nuchain server instances.

## Further Reading
1. While a little outdated, this post provides detailed instructions and goes further into the justifications for the
   above suggestions: <https://aws.amazon.com/blogs/apn/getting-started-with-ansible-and-dynamic-amazon-ec2-inventory-management/>
2. The official guide on how to use Ansible's AWS EC2 External Inventory Script:
   <https://docs.ansible.com/ansible/latest/user_guide/intro_dynamic_inventory.html#example-aws-ec2-external-inventory-script>
