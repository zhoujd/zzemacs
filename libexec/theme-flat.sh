#!/bin/bash


#  Install flat version theme
install_flat() {
  OLD_TITLE_1="@extend %titlebuttons;"
  OLD_TITLE_2="@extend %solid_titlebuttons;"
  OLD_TITLE_3="@extend %windows_button;" 
  NEW_TITLE=""

  ##  change titlebutton style
  cd gtk-3.0/sass
  cp -an _common.scss _common.scss.bak
  cp -an _apps.scss _apps.scss.bak
  sed -i "s/$OLD_TITLE_1/$NEW_TITLE/g" _common.scss
  sed -i "s/$OLD_TITLE_2/$NEW_TITLE/g" _common.scss
  sed -i "s/$OLD_TITLE_3/$NEW_TITLE/g" _apps.scss
}

restore_flat() {
  cd gtk-3.0/sass
  [[ -d _apps.scss.bak ]] && rm -rf _apps.scss
  [[ -d _common.scss.bak ]] && rm -rf _common.scss
  mv _apps.scss.bak _apps.scss
  mv _common.scss.bak _common.scss
  echo -e "Restore scss files ..."
}

install_flat
