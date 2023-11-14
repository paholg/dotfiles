#!/usr/bin/env ruby

PRE = "sudo nixos-container run vpn -- "

curl = "curl --connect-timeout 2 ipinfo.io 2> /dev/null | head -n2 | tail -n1"
def su(cmd)
  "su transmission -s $(which bash) -c '#{cmd}'"
end

def run(cmd)
  puts "Running '#{cmd}':"
  system(PRE + cmd)
end
  
cmds = [
  "systemctl start openvpn-pia",
  curl,
  su(curl),

  "systemctl stop openvpn-pia",
  curl,
  su(curl),
  "nft list ruleset",
]

run("systemctl status openvpn-pia")

cmds.each do |cmd|
  full_cmd = PRE + cmd
  res=`#{full_cmd}`
  puts "#{cmd}: #{res}"
end
