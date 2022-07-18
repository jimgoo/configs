# List processes running on a port
# Usage:
#    find-port-process.sh <PORT>

lsof -nP -iTCP -sTCP:LISTEN | grep $1
