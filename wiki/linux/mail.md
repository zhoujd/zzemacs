Mail
====

## Using Claws Mail with Gmail

```
[https://www.claws-mail.org/faq/index.php/Using_Claws_Mail_with_Gmail]
[claws-gmail](../../doc/linux/claws-gmail.pdf)
```

## Claws Mail Interface

```
[https://www.claws-mail.org/faq/index.php/Interface]
```

## Hotmail/Outlook

```
[https://support.microsoft.com/en-us/office/pop-imap-and-smtp-settings-8361e398-8af4-4e97-b147-6c6c4ac95353]
[https://www.howto-outlook.com/howto/accountsettings.htm]
Login -> Setting -> Sync email
IMAP Setting
Server name: outlook.office365.com
Port: 993
Encryption method: TLS

SMTP Setting
Server name: smtp.office365.com
Port: 587
Encryption method: STARTTLS
```

## Build claws mail

```
$ wget https://www.claws-mail.org/download.php?file=releases/claws-mail-4.0.0.tar.gz
$ tar xf claws-mail-4.0.0.tar.gz
$ cd claws-mail-4.0.0
$ sudo apt install libgnutls28-dev libgtk-3-dev libetpan-dev
$ ./configure --prefix=/usr/local/claws-mail
$ make j 4
$ sudo make install
```

## Send mail via msmtp

```
## https://marlam.de/msmtp/msmtp.html
## Install dependence
$ sudo apt install msmtp sharutils

## Basic
# Send Mail
$ cat <<EOF | msmtp ${MAIL[@]}
subject:
to:

EOF

## With attachements
$ cat <<EOF | \
    (cat - && uuencode /path/to/attachment1 attachment1.name) | \
    (cat - && uuencode /path/to/attachment2 attachment2.name) | \
    msmtp ${MAIL[@]}
subject:
to:

EOF
```

## Basic send mail via curl

```
## Install dependence
$ sudo apt install curl

## Basic
$ curl --ssl-reqd --url "smtps://$SERVER:$PORT" \
    --mail-from "$SENDER_ADDRESS" \
    --mail-rcpt "$RECIPIENT_ADDRESS1" \
    --mail-rcpt "$RECIPIENT_ADDRESS2" \
    --upload-file "$MAIL_CONTENT_FILE" \
    --user "$USER:$PASS"

## Example
$
curl "smtp://<smtp-server>:25" \
    --mail-from "from@mail.com" \
    --mail-rcpt "mail1@mail.com" \
    --mail-rcpt "mail2@mail.com" \
    --upload-file email_coverage.txt \
    --user "user@mail.com:PW"
```

## Send mail with attachment via curl

```
## https://www.baeldung.com/linux/curl-send-mail
$ cat xsendmail.sh <<EOF
#!/usr/bin/env bash

SERVER='smtp.example.com'
PORT='465'
USER='x@gerganov.com'
PASS='PASSWORD'
SENDER_ADDRESS="$1"
SENDER_NAME='SENDER'
RECIPIENT_NAME='y'
RECIPIENT_ADDRESS='y@gerganov.com'
SUBJECT='Scripted Mail'
MESSAGE=$'Line 1\nLine 2'
ATTACHMENT_FILE='/home/user/att1'
ATTACHMENT_TYPE="$(file --mime-type '$ATTACHMENT_FILE' | sed 's/.*: //')"

curl --ssl-reqd --url "smtps://$SERVER:$PORT" \
    --user "$USER:$PASS" \
    --mail-from "$SENDER_ADDRESS" \
    --mail-rcpt "$RECIPIENT_ADDRESS" \
    --header "Subject: $SUBJECT" \
    --header "From: $SENDER_NAME <$SENDER_ADDRESS>" \
    --header "To: $RECIPIENT_NAME <$RECIPIENT_ADDRESS>" \
    --form '=(;type=multipart/mixed' --form "=$MESSAGE;type=text/plain"
    --form "file=@$ATTACHMENT_FILE;type=$ATTACHMENT_TYPE;encoder=base64" --form '=)'
EOF
```
