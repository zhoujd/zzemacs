#!/bin/bash

## Fix: Cannot make directory '/var/run/screen': Permission denied

TARGET=/etc/profile.d/screen.sh

echo "Install SCREENDIR $TARGET"
sudo tee $TARGET <<EOF
#!/bin/bash
mkdir -p \$HOME/.local/run/screen
chmod 700 \$HOME/.local/run/screen
export SCREENDIR=\$HOME/.local/run/screen
EOF

echo "Install SCREENDIR Done"
