{ ... }: {
  system.stateVersion = "25.05";
  networking.hostName = "berry-rock5bplus-0";

  services.etcd = {
    listenPeerUrls = [ "https://192.168.1.80:2380" ];
    initialAdvertisePeerUrls = [ "https://192.168.1.80:2380" ];
    listenClientUrls = [ "https://192.168.1.80:2379" "https://127.0.0.1:2379" ];
    advertiseClientUrls = [ "https://192.168.1.80:2379" ];
  };

  sops.secrets.tls-key = {
    sopsFile = ../secrets/berry-rock5bplus-0/secrets.yaml;
  };
  environment.etc."tls-crt".source = ../secrets/berry-rock5bplus-0/cert.pem;
}
