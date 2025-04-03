def flat: [leaf_paths as $path | {"key": $path | join("."), "value": getpath($path)}] | from_entries;
def match(re): select((. != null) and (. | test(re)));
def nomatch(re): select(((. != null) and (. | test(re))) | not);

# log parsing stuff
def nh: select(.method!="grpc.health.v1.Health.Check" and .method!="grpc.health.v1.Health.Check ");

# kubectl jq stuff
def ports: .spec.containers[] | select(.ports != null) | {name, ports: .ports};
def decodeSecret: .data |= with_entries(.value |= @base64d);
def decodeSecret(f): .data |= with_entries(.value |= (@base64d | f));
def named(name): select(.metadata.name | test(name));
def restarted: {name: .metadata.name, phase: .status.phase, restarts: .status.containerStatuses[]} | select(.restarts.restartCount > 0);
