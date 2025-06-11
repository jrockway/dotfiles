{ ... }: {
  system.stateVersion = "25.05";
  networking.hostName = "berry-rock5bplus-0";

  services.etcd = {
    listenPeerUrls = [ "http://192.168.1.80:2380" ];
    initialAdvertisePeerUrls = [ "http://192.168.1.80:2380" ];
    listenClientUrls = [ "http://192.168.1.80:2379" "http://127.0.0.1:2379" ];
    advertiseClientUrls = [ "http://192.168.1.80:2379" ];
  };
}
