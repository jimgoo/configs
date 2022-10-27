set -e

# <TODO> set this
PUBLIC_KEY="$1"

chmod 700 ~/.ssh
echo $PUBLIC_KEY >> ~/.ssh/authorized_keys
chmod 700 ~/.ssh/authorized_keys
service ssh restart

apt-get update && apt-get install wget tmux emacs-nox htop -y

mkdir -p /workspace/git
cd /workspace/git
git clone https://github.com/jimgoo/configs.git
cd configs
cp .tmux.conf ~/

mv ~/.bashrc ~/.bashrc-orig
cp .bashrc .bash_prompt ~/
cp .emacs.el ~/

echo 'export PATH="/venv/bin:$PATH"' >> ~/.bashrc