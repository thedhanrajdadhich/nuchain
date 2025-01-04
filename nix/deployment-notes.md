# Servers
* ssh root@54.226.139.117       -- "Build" machine

* ssh root@54.166.153.21        -- Node 0

* ssh root@54.146.43.204        -- Node 1

* ssh root@34.204.71.247        -- Node 2

* ssh root@54.164.36.85         -- Node 3

# Server directories
* bin: /var/lib/nuchain/profiles/current/bin/nuchainserver

* working dir: /var/lib/nuchain/run

* conf files: /root/nuchain/10000-cluster.yaml, etc

* log files: /var/lib/nuchain/run/log

# ssh to each server and create directories:
* mkdir /var/lib/nuchain

* mkdir /var/lib/nuchain/profiles

* mkdir /var/lib/nuchain/run

* mkdir /var/lib/nuchain/run/log

* mkdir /root/nuchain

# SCP config to each node (from nuchain dir locally)
###    node0:
* scp nix/conf/* root@54.166.153.21:/root/nuchain/

###   node1:
* scp nix/conf/* root@54.146.43.204:/root/nuchain/

###    node2:
* scp nix/conf/* root@34.204.71.247:/root/nuchain/

###    node3:
* scp nix/conf/* root@54.164.36.85:/root/nuchain/

# nix-env to pull nuchain files
From each node (nix result taken from CI linux nix bulid):

* nix-env -p /var/lib/nuchain/profiles/current --set /nix/store/5lcq6s9baj33rljrydzdnp1y8940cc9s-nuchain-1.3.0.0

# Configure nuchain service
## Edit each node's /etc/nixos/configuration.nix:
### Add to the open port list:
*  900N (where N is the node number) and all four of 10000, 10001, 10002, 10002
* -- e.g.: networking.firewall.allowedTCPPorts = [ 80 443 9000 10000 10001 10002 10003];

and (using the correct yaml file name per server):
 ``` systemd.services.nuchain = {
    enable = true;
    description = "Nuchain Node";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    restartIfChanged = true;
    serviceConfig = {
      User = "root";
      KillMode = "process";
      WorkingDirectory = "/var/lib/nuchain/run";
      ExecStart = "/var/lib/nuchain/profiles/current/bin/nuchainserver +RTS -N4 -RTS -c /root/nuchain/1000N-cluster.yaml";
      Restart = "always";
      RestartSec = 5;
      LimitNOFILE = 65536;
    };
  };
```

## Run this for above service to be available:
* sudo -i nixos-rebuild switch

## Check system log for daemon startup errors:
* journalctl -u nuchain -f

# Check that nuchain is running (replace N with node #)
* tail -f /var/lib/nuchain/run/log/nodeN.log

# Restart all 4 nodes' nuchain service (from build server)
### from each node:
* systemctl restart nuchain

### or remotely:
* systemctl --host=root@54.166.153.21 restart nuchain
* systemctl --host=root@54.146.43.204 restart nuchain
* systemctl --host=root@34.204.71.247 restart nuchain
* systemctl --host=root@54.164.36.85 restart nuchain

# nix build automation next steps

1 - same steps as manual, automate as much file copying / editing / directory creating as possible

2 - Define minimal unique info for a given server, minimal unique info for a cluster, drive build from those
    sets of minimal information (including config file generation) -- 4 servers to start

N - generalize to N servers

3 - generate required keys on deployment (for nodes, for private 'entities', etc)
