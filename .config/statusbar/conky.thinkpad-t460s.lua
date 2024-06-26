local template = [[
^fg(\#aaaaaa)${exec countdown}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)↓${downspeedf IFACE}^fg(\#FFFFFF) ^fg(\#aaaaaa)↑${upspeedf IFACE}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)${exec monitor-client ping avg}ms ${exec monitor-client ping loss}% ${exec monitor-client location}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)/ ${exec monitor-client fs available /}^fg(\#FFFFFF) \
^fg(\#aaaaaa)/tmp ${exec monitor-client fs usage /tmp}%^fg(\#FFFFFF) \
^fg(\#aaaaaa)/home ${exec monitor-client fs available /home}^fg(\#FFFFFF) \
| ^fg(\#aaaaaa)C:${cpu}% T:${hwmon MON temp TEMP}° S:${swapperc}% M:${memperc}%^fg(\#FFFFFF) \
| ${battery_short BAT} (${battery_time BAT}) \
| ^fg(\#aaaaaa)${time %d-%m-%Y} ^fg(\#ebac54) ${time %R}
]]

local wlan = io.popen("wifi-device"):read()
template = template:gsub("IFACE", wlan)
template = template:gsub("BAT", "BAT0")
template = template:gsub("MON", 1):gsub("TEMP", 1)

conky.text = template
