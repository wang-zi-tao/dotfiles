io.write("sl:")
sl=tonumber(io.read())
h = fs.open("sl", "w")
h.write("sl='"..sl.."'")
h.close()
a=1
while true do
    if a <= sl then
        shell.run("wget http://alist.liulikeji.top/d/HFS/Smarthome/sb/lua.ywj /lua/"..a)
        shell.run("edit /lua/"..a)
        a=a+1
    else
        break;
    end
end
shell.run("wget http://alist.liulikeji.top/d/HFS/Smarthome/sb/startup.lua")
os.reboot()