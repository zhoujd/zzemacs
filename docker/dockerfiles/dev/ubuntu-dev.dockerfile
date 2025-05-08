ARG VARIANT=22.04
FROM zhoujd/ubuntu-${VARIANT}-zzemacs:base

## Setup apt-get
RUN sudo apt-get update

## Setup develop package
RUN sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        meld vim

RUN pip3 install --no-cache-dir \
        meson ninja

## Setup google-chrome
RUN wget -q https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
        && sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        ./google-chrome-stable_current_amd64.deb \
        && rm -f google-chrome-stable_current_amd64.deb \
        && echo -n "Chrome: " && google-chrome --version

## Setup vscode
RUN mkdir -p /etc/apt/keyrings \
        && curl -fsSL https://packages.microsoft.com/keys/microsoft.asc | sudo gpg --dearmor -o /usr/share/keyrings/vscode.gpg \
        && echo "deb [arch=amd64 signed-by=/usr/share/keyrings/vscode.gpg] https://packages.microsoft.com/repos/vscode stable main" \
        | sudo tee /etc/apt/sources.list.d/vscode.list \
        && sudo apt-get update \
        && sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        code

# Clean up APT when done.
RUN sudo apt-get clean \
        && sudo rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Setup entrypoint
COPY entrypoint.sh /app
ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["run"]
