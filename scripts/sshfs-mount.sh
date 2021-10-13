# Mount a remote file system with SSHFS.
# Usage:
#    sshfs-mount.sh <user>@<host> <mount_point> <ssh_key>
# where <ssh_key> is optional and defaults to ~/.ssh/id_rsa.

MOUNT_POINT="$2"
SSH_KEY="$3"

# parse <user>@<host> arguments
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

# echo "url: $url"
# echo "  proto: $proto"
# echo "  user: $user"
# echo "  pass: $pass"
# echo "  host: $host"
# echo "  port: $port"
# echo "  path: $path"

USERNAME=$user
IP=$host

if [ -z $SSH_KEY ]; then
    SSH_KEY=~/.ssh/id_rsa
fi

# create mount point directory
if [ ! -d $MOUNT_POINT ]; then
  echo "Creating mount point: $MOUNT_POINT"
  mkdir ${MOUNT_POINT}
fi

# unmount the mount point
umount -f ${MOUNT_POINT}

cmd="sshfs -o auto_cache,reconnect,defer_permissions,noappledouble,volname=${MOUNT_POINT},IdentityFile=$SSH_KEY ${USERNAME}@${IP}:/ ${MOUNT_POINT}"
echo $cmd
eval $cmd
