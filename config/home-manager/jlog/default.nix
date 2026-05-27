{
  pkgs,
  lib,
  jlog-src,
}:
pkgs.buildGoModule {
  pname = "jlog";
  version = "unstable-${jlog-src.shortRev or "dirty"}";
  src = jlog-src;
  vendorHash = "sha256-JBXtJLDcXfBnhBxiEL8GBDHAoD6sAJH9CpnWlDlNpO0=";
  subPackages = [ "cmd/jlog" ];
  ldflags = [
    "-s"
    "-w"
  ];
  meta = {
    description = "A pretty printer for JSON logs";
    homepage = "https://github.com/jrockway/json-logs";
    license = lib.licenses.mit;
    mainProgram = "jlog";
  };
}
