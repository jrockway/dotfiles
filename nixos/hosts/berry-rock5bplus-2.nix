{ ... }: {
  system.stateVersion = "25.05";
  networking.hostName = "berry-rock5bplus-2";

  services.etcd = {
    listenPeerUrls = [ "https://192.168.1.82:2380" ];
    initialAdvertisePeerUrls = [ "https://192.168.1.82:2380" ];
    listenClientUrls = [ "http://192.168.1.82:2379" "http://127.0.0.1:2379" ];
    advertiseClientUrls = [ "http://192.168.1.82:2379" ];
  };

  sops.secrets.tls-key = {
    sopsFile = ../secrets/berry-rock5bplus-2/secrets.yaml;
  };
  environment.etc."tls-crt".source = ../secrets/berry-rock5bplus-2/cert.pem;
}
