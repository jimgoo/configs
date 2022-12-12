# Keep an SSH connection alive with `autossh` and forward a port.
#
# Usage:
#    autossh-tunnel.sh <user>@<host>:<port> <targetPort=22> <sshKeyPath=~/.ssh/id_rsa>

proto="$(echo $1 | grep :// | sed -e's,^\(.*://\).*,\1,g')"
# remove the protocol
url="$(echo ${1/$proto/})"
# extract the user (if any)
userpass="$(echo $url | grep @ | cut -d@ -f1)"
pass="$(echo $userpass | grep : | cut -d: -f2)"
if [ -n "$pass" ]; then
  user="$(echo $userpass | grep : | cut -d: -f1)"
else
    user=$userpass
fi

if [ $# -eq 2 ]; then
  sshPort="$2"
else
  sshPort="22"
fi
echo "sshPort: $sshPort"

if [ $# -eq 3 ]; then
  sshKey="$3"
else
  sshKey="~/.ssh/id_rsa"
fi
echo "sshKey: $sshKey"

# extract the host
host="$(echo ${url/$user@/} | cut -d/ -f1)"
# by request - try to extract the port
port="$(echo $host | sed -e 's,^.*:,:,g' -e 's,.*:\([0-9]*\).*,\1,g' -e 's,[^0-9],,g')"
# extract the path (if any)
path="$(echo $url | grep / | cut -d/ -f2-)"

# remove port from host
host=${host/:$port}

cmd='autossh -M 0 -o "ServerAliveInterval 10" -o "ServerAliveCountMax 3" -Y -L $port:localhost:$port $user@$host -p $sshPort -i $sshKey'
echo $cmd
eval $cmd

