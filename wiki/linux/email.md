E-mail
======

## Using Claws Mail with Gmail

[https://www.claws-mail.org/faq/index.php/Using_Claws_Mail_with_Gmail]
[claws-gmail](../../doc/linux/claws-gmail.pdf)


## Claws Mail Interface
[https://www.claws-mail.org/faq/index.php/Interface]

## Hotmail/Outlook

Login -> Setting -> Sync email
IMAP Setting
Server name: outlook.office365.com
Port: 993
Encryption method: TLS

SMTP Setting
Server name: smtp.office365.com
Port: 587
Encryption method: STARTTLS

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
