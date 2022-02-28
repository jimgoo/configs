set -e

# For linux

sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs27
sudo update-alternatives --config emacs
