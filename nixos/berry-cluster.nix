{ ... }: {
  services.flannel = {
    enable = true;
    network = "192.168.32.0/19";
    iface = "enP4p65s0";
    backend = { Type = "host-gw"; };
    etcd = {
      endpoints = [
        "https://192.168.1.80:2379"
        "https://192.168.1.81:2379"
        "https://192.168.1.82:2379"
      ];
      keyFile = "/run/secrets/tls-key";
      certFile = "/etc/tls-crt";
    };
  };

  services.etcd = {
    enable = true;
    initialCluster = [
      "berry-rock5bplus-0=https://192.168.1.80:2380"
      "berry-rock5bplus-1=https://192.168.1.81:2380"
      "berry-rock5bplus-2=https://192.168.1.82:2380"
    ];
    keyFile = "/run/secrets/tls-key";
    certFile = "/etc/tls-crt";
    peerClientCertAuth = true;
    clientCertAuth = true;
    openFirewall = true;
  };

  users.users.etcd.extraGroups = [ "keys" "tls" ];
  sops.secrets.tls-key = {
    mode = "0440";
    group = "tls";
  };
}
