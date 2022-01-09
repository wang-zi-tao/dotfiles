if xdotool search --class 'eww-control-center'
then
 eww close control-center
else
 eww update control-center-enable=true
 eww open control-center
fi
