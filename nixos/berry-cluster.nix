{ ... }: {
  services.flannel = {
    enable = true;
    network = "192.168.32.0/19";
    interface = "enP4p65s0";
    backend = "host-gw";
    etcd.endpoints = [
      "http://192.168.1.80:2379"
      "http://192.168.1.81:2379"
      "http://192.168.1.82:2379"
    ];
  };

  services.etcd = {
    enable = true;
    initialCluster = [
      "berry-rock5bplus-0=http://192.168.1.80:2380"
      "berry-rock5bplus-1=http://192.168.1.81:2380"
      "berry-rock5bplus-2=http://192.168.1.82:2380"
    ];
    openFirewall = true;
  };
}
