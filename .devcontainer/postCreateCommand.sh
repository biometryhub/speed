sudo apt-get -y update
sudo apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev libglpk-dev

export QUARTO_VERSION="1.7.33"
sudo mkdir -p /opt/quarto/${QUARTO_VERSION}
sudo curl -o quarto.tar.gz -L \
    "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.tar.gz"
sudo tar -zxvf quarto.tar.gz \
    -C "/opt/quarto/${QUARTO_VERSION}" \
    --strip-components=1
sudo rm quarto.tar.gz
sudo ln -s /opt/quarto/${QUARTO_VERSION}/bin/quarto /usr/local/bin/quarto
sudo apt install -y qpdf

# libgfortran.so.3 for prebuilt benchmark binaries (odw, oldDiGGer)
GCC6_URL="http://archive.ubuntu.com/ubuntu/pool/universe/g/gcc-6"
curl -LO "${GCC6_URL}/gcc-6-base_6.4.0-17ubuntu1_amd64.deb"
curl -LO "${GCC6_URL}/libgfortran3_6.4.0-17ubuntu1_amd64.deb"
sudo dpkg -i gcc-6-base_6.4.0-17ubuntu1_amd64.deb libgfortran3_6.4.0-17ubuntu1_amd64.deb
sudo ldconfig
rm gcc-6-base_6.4.0-17ubuntu1_amd64.deb libgfortran3_6.4.0-17ubuntu1_amd64.deb
