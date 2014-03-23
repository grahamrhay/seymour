sudo apt-get update
sudo apt-get upgrade -qy

# install Erlang R16B03-1
sudo apt-get install build-essential libncurses5-dev openssl libssl-dev fop xsltproc unixodbc-dev git -qy
wget -q http://erlang.org/download/otp_src_R16B03-1.tar.gz
tar zxvf otp_src_R16B03-1.tar.gz
cd otp_src_R16B03-1
./configure && make && sudo make install
