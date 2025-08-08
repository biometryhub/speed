sudo apt-get -y update
sudo apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev libglpk-dev

# https://gist.github.com/sakethramanujam/faf5b677b6505437dbdd82170ac55322
wget http://archive.ubuntu.com/ubuntu/pool/universe/g/gcc-6/gcc-6-base_6.4.0-17ubuntu1_amd64.deb
wget http://archive.ubuntu.com/ubuntu/pool/universe/g/gcc-6/libgfortran3_6.4.0-17ubuntu1_amd64.deb

sudo dpkg -i gcc-6-base_6.4.0-17ubuntu1_amd64.deb
sudo dpkg -i libgfortran3_6.4.0-17ubuntu1_amd64.deb

rm gcc-6-base_6.4.0-17ubuntu1_amd64.deb
rm libgfortran3_6.4.0-17ubuntu1_amd64.deb
