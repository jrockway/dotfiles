{ ... }: {
  system.stateVersion = "25.05";
  networking.hostName = "berry-rock5bplus-1";

  services.etcd = {
    listenPeerUrls = [ "http://192.168.1.81:2380" ];
    initialAdvertisePeerUrls = [ "http://192.168.1.81:2380" ];
    listenClientUrls = [ "http://192.168.1.81:2379" "http://127.0.0.1:2379" ];
    advertiseClientUrls = [ "http://192.168.1.81:2379" ];
  };
}
