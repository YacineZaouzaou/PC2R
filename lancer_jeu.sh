#!/bin/bash


if [ $# -lt 3 ] 
then 
    echo "erreur nombre de paramètre"
fi

./serverC.exe $2 &

i=3

while [ $i -le $# ] 
do
    echo "lancement du joueur' $i"
    $1 '-jar' 'client.jar' $2 $$i &
    let i++
    sleep .7
done

