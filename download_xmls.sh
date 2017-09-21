#!/bin/bash

site=https://www.orpheus-verein.de/

mkdir xmls || false

echo "Dein Benutzername:"
read name
echo "Dein Passwort:"
read pass
echo "ID des Seminars:"
read id

cookies=/tmp/orpheus-cookies.txt
wget -O /dev/null --save-cookies /tmp/ba-cookies.txt --keep-session-cookies --load-cookies $cookies "${site}user"
wget --keep-session-cookies --save-cookies $cookies --load-cookies $cookies -O /dev/null \
        --post-data="name=$name&pass=$pass&op=Log%20in&form_id=user_login" \
        "${site}user?destination=login_redirect"
timestamp=`date -I'seconds'`
cd xmls
mkdir ${timestamp}
cd ${timestamp}
for xmlfile in zeiteinheiten alle-voraussetzungen räume teilnehmer-und-betreuer themenauswahl themenwahlen verpassen raum-nicht-verfügbar muss-stattfinden-an
do
	wget --keep-session-cookies --save-cookies $cookies --load-cookies $cookies "${site}orfeo/${id}/${xmlfile}.xml" 
done
