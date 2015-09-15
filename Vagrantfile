# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.box = "fgrehm/trusty64-lxc"

  config.vm.provision "shell", inline: <<-SHELL
    sudo apt-get install -y libgmp10
    sudo /vagrant/main
  SHELL
end
