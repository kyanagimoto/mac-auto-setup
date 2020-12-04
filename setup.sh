#!/bin/bash

function command_exists {
  command -v "$1" > /dev/null;
}

#
# Copy git ssh config file
#
echo " ------- Git SSH config ------"
cp $(cd $(dirname ${BASH_SOURCE:-$0}); pwd)/settings/git/config ~/.ssh/config
while true; do
  read -p 'Now git ssh settings? [Y/n]' Answer
  case $Answer in
    '' | [Yy]* )
      ssh-keygen -t rsa
      chmod 600 ~/.ssh/id_rsa
      eval `ssh-agent`
      ssh-add ~/.ssh/id_rsa
      ssh-add -l
      echo "Let’s register your public key on GitHub"
      break;
      ;;
    [Nn]* )
      echo "Skip settings"
      break;
      ;;
    * )
      echo Please answer YES or NO.
  esac
done;
echo " ------------ END ------------"

#
# Memorize user pass
#
read -sp "Your Password: " pass;

#
# Mac App Store apps install
#
if ! command_exists mas ; then
  echo " ---- Mac App Store apps -----"
  brew install mas
  mas install 497799835  # Xcode (8.2.1)
  echo " ------------ END ------------"
fi

#
# Install zsh
#
if ! command_exists zsh ; then
  echo " ------------ zsh ------------"
  brew install zsh zsh-autosuggestions zsh-completions zsh-syntax-highlighting colordiff
  which -a zsh
  echo $pass | sudo -S -- sh -c 'echo '/usr/local/bin/zsh' >> /etc/shells'
  chsh -s /usr/local/bin/zsh
  echo " ------------ END ------------"
fi

#
# Install vim
#
if ! command_exists vim ; then
  echo " ------------ Vim ------------"
  brew unlink macvim
  brew install vim
  echo " ------------ END ------------"
fi

#
# Powerline
#
echo " --------- Powerline ---------"
# Font is 14pt Iconsolata for Powerline with Solarized Dark iterm2 colors.
git clone https://github.com/bhilburn/powerlevel9k.git ~/powerlevel9k
git clone https://github.com/powerline/fonts.git ~/fonts
~/fonts/install.sh
echo " ------------ END ------------"

#
# install anyenv
#
if ! command_exists anyenv ; then
  echo "----------- Anyenv -------------"
  brew install anyenv
  echo 'eval "$(anyenv init -)"' >> ~/.zshrc
  anyenv init
  anyenv install --init
  mkdir -p $(anyenv root)/plugins
  git clone https://github.com/znz/anyenv-update.git $(anyenv root)/plugins/anyenv-update
  anyenv update
fi

#
# Install ruby
#
if ! command_exists rbenv ; then
  echo " ----------- Ruby ------------"
  anyenv install rbenv
  rbenv --version
  rbenv install -l
  ruby_latest=$(rbenv install -l | grep -v '[a-z]' | tail -1 | sed 's/ //g')
  rbenv install $ruby_latest
  rbenv global $ruby_latest
  rbenv rehash
  ruby -v
  echo " ------------ END ------------"
fi

#
# Install dotfiles system
#
echo " ---------- dotfiles ---------"
sh -c "`curl -fsSL https://raw.githubusercontent.com/skwp/dotfiles/master/install.sh`"
cp ./settings/zsh/private.zsh ~/.yadr/zsh/private.zsh
source ~/.zshrc
echo " ------------ END ------------"

#
# Install Node.js env
if ! command_exists nodebrew ; then
  echo " ---------- Node.js ----------"
  curl -L git.io/nodebrew | perl - setup
  nodebrew ls-remote
  nodebrew install-binary latest
  nodebrew ls
  nodebrew use latest
  node -v
  npm -v
  echo " ------------ END ------------"
fi

#
# Install Yarn
#
if ! command_exists yarn ; then
  echo " ----------- Yarn ------------"
  brew install yarn
  echo " ------------ END ------------"
fi

#
# Install wget
#
if ! command_exists wget ; then
  echo " ----------- wget ------------"
  brew install wget
  wget --version
  echo " ------------ END ------------"
fi

#
# CocoaPods
#
if ! command_exists pod ; then
  echo " --------- CocoaPods ---------"
  echo $pass | sudo -S gem install -n /usr/local/bin cocoapods --pre
  pod setup
  echo " ------------ END ------------"
fi

#
# Carthage
#
if ! command_exists carthage ; then
  echo " --------- Carthage ----------"
  brew install carthage
  echo " ------------ END ------------"
fi

#
# Install git-cz
#
if ! command_exists git-cz ; then
  echo " ---------- git-cz ---------- "
  npm install -g git-cz
  echo " ---------- END ----------"
fi

#
# Install asdf
#
if ! command_exists asdf ; then
    echo " ---------- asdf ---------- "
    brew install asdf
    echo '. $(brew --prefix asdf)/asdf.sh' >> ~/.zshrc
    echo " ---------- END ---------- "
fi

#
# Install kubectl
#
if ! command_exists kubectl ; then
    echo " ---------- kubectl ---------- "
    asdf plugin-add kubectl
    asdf install kubectl 1.15.10
    asdf global kubectl 1.15.10
    kubectl version
    echo " ---------- END ----------"
fi

#
# Install kubectx
#
if ! command_exists kubectx ; then
    echo " ---------- kubectx ---------- "
    brew install kubectx
    echo " ---------- END ----------- "
fi

#
# Install fzf
#
if ! command_exists fzf ; then
    echo " ---------- fzf ---------- "
    brew install fzf
    $(brew --prefix)/opt/fzf/install
    echo " ---------- END ---------- "
fi

#
# Install azure-cli
#
if ! command_exists az ; then
    echo " ---------- az ---------- "
    brew install azure-cli
    echo " ---------- END ---------- "
fi

#
# Install peco
#
if ! command_exists peco ; then
    echo " ---------- peco ---------- "
    brew install peco
    echo " ---------- END ---------- "
fi

#
# Install terraform
#
if ! command_exists terraform ; then
    echo " ---------- terraform ---------- "
    anyenv install tfenv
    tfenv install latest
    tfenv use latest
    echo " ---------- END ---------- "
fi

#
# Install go
#
if ! command_exists go ; then
    echo " ---------- go ---------- "
    anyenv install goenv
    exec $SHELL -l
    goenv install 1.14.2
    echo " ---------- END ---------- "
fi

#
# Install jq
#
if ! command_exists jq ; then
    echo " ---------- jq ---------- "
    brew install jq
    echo " ---------- END ---------- "
fi

#
# Install stern
#
if ! command_exists stern ; then
    echo "----------- stern -------------"
    brew install stern
    echo " ---------- END ---------- "
fi

while true; do
  read -p 'Now install web apps? [Y/n]' Answer
  case $Answer in
    '' | [Yy]* )
      $(cd $(dirname ${BASH_SOURCE:-$0}); pwd)/app.sh
      break;
      ;;
    [Nn]* )
      echo "Skip install"
      break;
      ;;
    * )
      echo Please answer YES or NO.
  esac
done;

while true; do
  read -p 'Now install App Store apps? [Y/n]' Answer
  case $Answer in
    '' | [Yy]* )
      $(cd $(dirname ${BASH_SOURCE:-$0}); pwd)/appstore.sh
      break;
      ;;
    [Nn]* )
      echo "Skip install"
      break;
      ;;
    * )
      echo Please answer YES or NO.
  esac
done;

read -p 'Please enter your GitHub Access Token. You can skip by typing "N".' Answer
case $Answer in
  '' | [Nn]* )
    echo "Skip"
    ;;
  * )
    echo "export GITHUB_ACCESS_TOKEN=${Answer}" >> ~/.yadr/zsh/private.zsh
    echo "export HOMEBREW_GITHUB_API_TOKEN=${Answer}" >> ~/.yadr/zsh/private.zsh
    echo "Writing to ~/.yadr/zsh/private.zsh is complete."
    echo " ------------ END ------------"
esac
