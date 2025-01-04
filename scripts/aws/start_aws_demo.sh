#!/bin/bash

OS='ubuntu-16.04'
EC2_USER='ubuntu'

sudo chmod +x bin/$OS/nuchainclient
ansible-playbook aws/run_servers.yml &&
STR_SERVERS=`cat aws/ipAddr.yml`
SERVERS=( $STR_SERVERS )

tmux new-window &&

tmux split-window -h &&
tmux send-keys "ssh -t -A $EC2_USER@${SERVERS[0]} tail -f log/${SERVERS[0]}.log" C-m &&
sleep 1 &&

tmux split-window -v -p 75 &&
tmux send-keys "ssh -t -A $EC2_USER@${SERVERS[1]} tail -f log/${SERVERS[1]}.log" C-m &&
sleep 1 &&

tmux split-window -v -p 66 &&
tmux send-keys "ssh -t -A $EC2_USER@${SERVERS[2]} tail -f log/${SERVERS[2]}.log" C-m &&
sleep 1 &&

tmux split-window -v -p 50 &&
tmux send-keys "ssh -t -A $EC2_USER@${SERVERS[3]} tail -f log/${SERVERS[3]}.log" C-m &&
sleep 1 &&

tmux select-pane -L
tmux send-keys "bin/$OS/nuchainclient.sh"
