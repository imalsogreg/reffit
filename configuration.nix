{ config, pkgs, ... }:

{
  imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];
  ec2.hvm = true;


  nixpkgs.config.allowBroken = true;
  nixpkgs.config.allowUnfree = true;
  nix.trustedBinaryCaches = [ "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

  networking.firewall.allowedTCPPorts = [80 443 6667 8000 8001 8080 24800];
  networking.extraHosts =
    ''
    '';

  security.sudo.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };


  # Set your time zone.
  time.timeZone = "US/Eastern";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    curl
    git
    gitAndTools.gitFull
    gnupg
    haskellPackages.cabal2nix
    tmux
    traceroute
    vim
    wget
    xclip
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # services.synergy.server = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.greghale = {
    isNormalUser = true;
    uid = 1000;
    description = "Greg Hale";
    extraGroups = [ "wheel" "networkmanager" ];
  };

  users.extraGroups.vboxusers.members = [ "greghale" ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";

  # nginx and letsencrypt

  security.acme.certs."reffit.com" = {
    webroot = "/var/www/challenges";
    email = "imalsogreg@gmail.com";
  }; 

  services.nginx = {
    enable=true;
    virtualHosts = {
      "reffit.com" = {
        forceSSL = false;
        enableACME = true;
        locations."/" = {
          root = "/var/www";
        };
      };
    };
  };

  services.nginx.httpConfig = ''
    server {
      server_name          reffit.com;
      listen               80;
      listen               [::]:80;
      client_max_body_size 100M;

      location /.well-known/acme-challenge {
        root /var/www/challenges;
      }

      location / {
        return 301 https://$host$request_uri;
      }

    }

    server {
      server_name reffit.com;
      listen 443 ssl;
      client_max_body_size 100M;

      ssl_certificate     ${config.security.acme.directory}/reffit.com/fullchain.pem;
      ssl_certificate_key ${config.security.acme.directory}/reffit.com/key.pem;

      location /talks/bayhack2017 {
        return 301 http://bayhack2017.s3-website-us-west-1.amazonaws.com;
        # proxy_pass http://bayhack2017.s3-website-us-west-1.amazonaws.com;
      }

      location / {
        proxy_pass http://127.0.0.1:8000;
      }

    }

  '';

}
