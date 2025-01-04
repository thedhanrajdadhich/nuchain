#!/bin/bash

if [ -d "aws-conf" ]
then
    rm -rf aws-conf/*
else
    mkdir aws-conf
fi

aws ec2 describe-instances --filter Name=tag:Name,Values=nuchainserver \
  | grep '"PrivateIpAddress"' \
  | sed 's/[^(0-9).]//g' \
  | uniq | sort > aws-conf/nuchainservers.privateIp

aws ec2 describe-instances --filter Name=tag:Name,Values=nuchainclient \
  | grep '"PrivateIpAddress"' \
  | sed 's/[^(0-9).]//g' \
  | uniq | sort > aws-conf/nuchainclient.privateIp

./bin/genconfs --distributed aws-conf/nuchainservers.privateIp

for ip in `cat aws-conf/nuchainservers.privateIp`; do
    idir="aws-conf/${ip}"
    mkdir -p $idir/conf
    conf="${ip}-cluster.yaml"
    script="${idir}/start.sh"
    mv ./conf/$conf $idir/conf/$conf
    echo "#!/bin/sh
nohup ./nuchainserver +RTS -N -RTS -c conf/${conf} >> ./${ip}-output.log 2>&1 &
" > $script
    chmod +x $script
done

echo 'make sure you have run `stack install` from nuchain and have `~/.local/bin` in your path (or have run `cd ~ ; ln -s .local/bin/* . `)'
