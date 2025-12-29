{ ... }: {

services.invidious = {
  enable = true;
  domain = "invidious.cloverp.duckdns.org";
  nginx.enable = true;
};

}
