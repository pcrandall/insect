# Native client for insect

See [Insect](https://github.com/sharkdp/insect) for more details.

Bundled with [Neutralinojs](https://github.com/neutralinojs/neutralinojs)

## Installation

Install Darwin

    curl -s https://api.github.com/repos/pcrandall/insect/releases/latest | grep "browser_download_url" | cut -d '"' -f 4 | grep darwin | wget -qi - --output-document=insect.tar.gz && tar -xf insect.tar.gz && rm insect.tar.gz

Install Windows

    curl -s https://api.github.com/repos/pcrandall/insect/releases/latest | grep "browser_download_url" | cut -d '"' -f 4 | grep windows | wget -qi - --output-document=insect.zip && unzip -o insect.zip && rm insect.zip

Install Linux

    curl -s https://api.github.com/repos/pcrandall/insect/releases/latest | grep "browser_download_url" | cut -d '"' -f 4 | grep linux | wget -qi - --output-document=insect.tar.gz && tar -xf insect.tar.gz && rm insect.tar.gz

