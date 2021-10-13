# Keep an SSH connection alive with `autossh` and forward a port.
#
# Usage:
#    autossh-tunnel.sh <user>@<host>:<port>

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

# extract the host
host="$(echo ${url/$user@/} | cut -d/ -f1)"
# by request - try to extract the port
port="$(echo $host | sed -e 's,^.*:,:,g' -e 's,.*:\([0-9]*\).*,\1,g' -e 's,[^0-9],,g')"
# extract the path (if any)
path="$(echo $url | grep / | cut -d/ -f2-)"

# remove port from host
host=${host/:$port}

cmd='autossh -M 0 -o "ServerAliveInterval 10" -o "ServerAliveCountMax 3" -Y -L $port:localhost:$port $user@$host'
echo $cmd
eval $cmd

