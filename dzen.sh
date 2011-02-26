#!/bin/zsh
#
# Dzen statusbar, compiled from various sources
# Requires weather.com key and mocp
##################################################################

# Configuration
##################################################################
# Dzen's font

MAIN_COLOR="#00aa00"
DZENFNT="-*-snap-*-*-*-*-12-*-*-*-*-*-*-*"
# Dzen's background colour
DZENBG='#262626'
# Dzen's forground colour
DZENFG='#999999'
# Dzen's X position
DZENX=600
# Dzen's Y posit
DZENY=0
# Dzen's width
DZENWIDTH=766
# Dzen's alignment (l=left c=center r=right)
DZENALIGN=r
# Gauge background colour
GAUGEBG='#323232'
# Gauge foreground colour
GAUGEFG='#0069e0'
# Path to your Dzen icons
ICONPATH=/home/pielgrzym/.xmonad/icons
# Network interface
INTERFACE=wlan0
# Sound device for volume control
SNDDEVICE=Master
# Date formating
# Main loop interval in seconds
SLEEP=1

# Function calling intervals in seconds
DATEIVAL=20
CPUTEMPIVAL=60
VOLUMEIVAL=1
BATIVAL=5
WIFIIVAL=5
##################################################################

# battery >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fbattery() {
        GW=30      # width of the gauge
        GFG='#999'  # color of the gauge
        GH=7       # height of the gauge
        GBG='#333'  # color of gauge background

        STATEFILE='/proc/acpi/battery/BAT0/state' # battery's state file
        INFOFILE='/proc/acpi/battery/BAT0/info'   # battery's info file

        LOWBAT=3        # percentage of battery life marked as low
        LOWCOL='#ff4747' # color when battery is low

        PREBAR="^i(${ICONPATH}/bat_full_01.xbm)" # caption (also icons are possible)

        # look up battery's data
        BAT_FULL=`cat $INFOFILE|grep design|line|cut -d " " -f 11`
        STATUS=`cat $STATEFILE|grep charging|cut -d " " -f 12`
        RCAP=`cat $STATEFILE|grep remaining|cut -d " " -f 8`

        # calculate remaining power
        RPERCT=`expr $RCAP \* 100`;
        RPERC=`expr $RPERCT / $BAT_FULL`

        # draw the bar and pipe everything into dzen
        if [ $RPERC -le $LOWBAT ]; then 
                GFG=$LOWCOL
                aplay ~/var/bateria.wav
        fi
        echo -n "$PREBAR $RPERC% "
        echo $RPERC | gdbar -s o -ss 1 -sw 1 -h $GH -w $GW -fg $GFG -bg $GBG
}

# wifi
##################################################################

fwifi() {
        WIFI_STATE=`cat /sys/class/net/wlan0/operstate`
        WIFI_SIGNAL=`cat /sys/class/net/wlan0/wireless/link`
        if [[ $WIFI_STATE == "up" ]]; then
                echo -n "^i($ICONPATH/wifi_02.xbm) "
                echo $WIFI_SIGNAL | gdbar -s o -ss 1 -sw 1 -w 30 -h 7 -fg $GAUGEFG -bg $GAUGEBG
        fi
}


# Time and date 
##################################################################

fdate() {
        date +"%d.%m.%Y"
}

ftime() {
        date +"%H:%M"
}
##################################################################



# CPU use
##################################################################
fcpu() {
        gcpubar -c 5 -i 0.1 -s o -ss 1 -sw 1 -fg $GAUGEFG -bg $GAUGEBG -h 7 -w 30 | tail -1
}
##################################################################


# CPU temp   
##################################################################
fcputemp() {
        print -n ${(@)$(</proc/acpi/thermal_zone/TZ00/temperature)[2,3]}
}
##################################################################


# HD partitions used and free space
##################################################################
#fhd() {
# Todo
#}
##################################################################

# Network
##################################################################

# Here we remember the previous rx/tx counts
RXB=`cat /sys/class/net/${INTERFACE}/statistics/rx_bytes`
TXB=`cat /sys/class/net/${INTERFACE}/statistics/tx_bytes`


# MOCP song info and control
##################################################################
#fmusic() {
#artist=`mocp -i | grep 'Artist' | cut -c 8-`
#songtitle=`mocp -i | grep 'SongTitle' | cut -c 11-`
#totaltime=`mocp -i | grep 'TotalTime' | cut -c 12-`
#currenttime=`mocp -i | grep 'CurrentTime' | cut -c 14-`
#state=`mocp -i | grep 'State' | cut -c 8-`
#print -n "$(echo $artist -$songtitle [)$(echo $currenttime/$totaltime] [)$(echo $state])"
#}

# For Creative Audigy 2 ZS
fvolume() {
        percentage=`amixer sget Master | sed -ne 's/^.*Mono: .*\[\([0-9]*\)%\].*$/\1/p'`
        print -n "$(echo $percentage | gdbar -s v -fg $GAUGEFG -bg $GAUGEBG -h 7 -w 6)"
}


# Command to toggle pause/unpause
TOGGLE="mocp -G"
# Command to increase the volume
CI="amixer -c0 sset Master 2dB+ >/dev/null"
# Command to decrease the volume
CD="amixer -c0 sset Master 2dB- >/dev/null"
##################################################################


# Main function
##################################################################

DATECOUNTER=0;CPUTEMPCOUNTER=0;VOLUMECOUNTER=0;BATTCOUNTER=0;WIFICOUNTER=0;

# Execute everything once
PDATE=$(fdate)
PTIME=$(ftime)
PCPU=$(fcpu)
PVOLUME=$(fvolume)
#BATTERY=$(fbattery)
#WIFI=$(fwifi)

# Main loop
while :; do

        PCPU=$(fcpu)

        if [ $WIFICOUNTER -ge $WIFIIVAL ]; then
                WIFI=$(fwifi)
                WIFICOUNTER=0
        fi

        if [ $BATTCOUNTER -ge $BATIVAL ]; then
                BATTERY=$(fbattery)
                BATTCOUNTER=0
        fi

        if [ $DATECOUNTER -ge $DATEIVAL ]; then
                PDATE=$(fdate)
                PTIME=$(ftime)
                DATECOUNTER=0
        fi

        if [ $VOLUMECOUNTER -ge $VOLUMEIVAL ]; then
                PVOLUME=$(fvolume)
                VOLUMECOUNTER=0
        fi

        # Get new rx/tx counts
        RXBN=`cat /sys/class/net/${INTERFACE}/statistics/rx_bytes`
        TXBN=`cat /sys/class/net/${INTERFACE}/statistics/tx_bytes`

        # Calculate the rates
        # format the values to 4 digit fields
        RXR=$(printf "%d\n" $(echo "($RXBN - $RXB) / 1024/${SLEEP}" | bc))
        TXR=$(printf "%d\n" $(echo "($TXBN - $TXB) / 1024/${SLEEP}" | bc))

        # Print out 
        echo " ^fg($MAIN_COLOR)^p(0)^i(${ICONPATH}/cpu.xbm) ^fg()${PCPU} ^fg(#80AA83)^p(0)^i(${ICONPATH}/net_down_02.xbm)^fg()${RXR}kB/s ^fg(orange3)^p(0)^i(${ICONPATH}/net_up_02.xbm)^fg()${TXR}kB/s^fg() ^fg() ^fg($MAIN_COLOR)^p(0)^i(${ICONPATH}/spkr_01.xbm) ${PVOLUME} ^fg(#FFFFFF)${PDATE} ^bg($MAIN_COLOR) ${PTIME} "

        # Reset old rates
        RXB=$RXBN; TXB=$TXBN

        DATECOUNTER=$((DATECOUNTER+1))
        CPUTEMPCOUNTER=$((CPUTEMPCOUNTER+1))
        VOLUMECOUNTER=$((VOLUMECOUNTER+1))
        BATTCOUNTER=$((BATTCOUNTER+1))
        WIFICOUNTER=$((WIFICOUNTER+1))

        sleep $SLEEP

        # Pass it to dzen
done | dzen2 -xs 0 -bg $DZENBG -fg $DZENFG -x $DZENX -y $DZENY -ta $DZENALIGN -h 14 -p -e "button2=exec:$TOGGLE;button4=exec:$CI;button5=exec:$CD" -fn $DZENFNT
