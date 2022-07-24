pkgs: prev:
with builtins; with pkgs.lib;with pkgs.lib.types; with pkgs.lib.attrsets; let
in
{
  graphType = { nodeOption ? { ... }: { }, edgeOption ? { ... }: { }, defaultEdgeConfig ? { }, directed ? false, }:
    let
      type = submodule {
        options = {
          defaultEdge = mkOption { type = anything; default = { }; };
          nodes = mkOption {
            type = attrsOf (submodule (nodeArgs@{ name, config, ... }:
              let
                shortEdgeOption = mkOption {
                  type = listOf str;
                  default = [ ];
                };
                longEdgeOption = mkOption {
                  type = attrsOf (submodule
                    ({ name, config, ... }: {
                      options = edgeOption
                        ({ inherit name config; nodeName = nodeArgs.name; nodeConfig = nodeArgs.config; });
                    }));
                  default = defaultEdgeConfig;
                };
              in
              {
                options = {
                  config = nodeOption { nodeName = name; nodeConfig = config; };

                  from_node = shortEdgeOption;
                  peer_node = shortEdgeOption;
                  to_node = shortEdgeOption;

                  from = longEdgeOption;
                  peers = longEdgeOption;
                  to = longEdgeOption;

                  from_full = longEdgeOption;
                  peers_full = longEdgeOption;
                  to_full = longEdgeOption;
                };
              }));
            default = { };
          };
          edges = mkOption {
            type = attrsOf (submodule (nodeArgs@{ name, config, ... }:
              let
                longEdgeOption = mkOption {
                  type = attrsOf (submodule
                    ({ name, config, ... }: {
                      options = edgeOption
                        ({ inherit name config; nodeName = nodeArgs.name; nodeConfig = nodeArgs.config; });
                    }));
                  default = defaultEdgeConfig;
                };
              in
              {
                options = {
                  config = nodeOption { nodeName = name; nodeConfig = config; };
                  from = longEdgeOption;
                  peers = longEdgeOption;
                  to = longEdgeOption;
                };
              }));
            default = [ ];
          };
          reflexive = mkOption {
            type = bool;
            default = false;
          };
          fromWhiteList = mkOption {
            type = listOf str;
            default = [ ];
          };
          fromBlackList = mkOption {
            type = nullOr (listOf str);
            default = null;
          };
          toWhiteList = mkOption {
            type = listOf str;
            default = [ ];
          };
          toBlackList = mkOption {
            type = nullOr (listOf str);
            default = null;
          };
          peersWhiteList = mkOption {
            type = listOf str;
            default = [ ];
          };
          peersBlackList = mkOption {
            type = nullOr (listOf str);
            default = null;
          };
          checkFunction = mkOption {
            type = anything;
            default = null;
          };
        };
      };
      function = input:
        let
          nodeList = attrNames input.nodes;
        in
        {
          edges = mapAttrs
            (nodeName: node:
              let
                from_nodes = node.from_node
                  ++ (attrNames (filterAttrs
                  (name: other_node: elem nodeName other_node.to_node)
                  input.nodes))
                  ++ (
                  if input.fromBlackList != null then
                    filter (other_node: !elem other_node input.fromBlackList) nodeList
                  else input.fromWhiteList
                ) ++ optionals ((input.toBlackList == null || !elem nodeName input.toBlackList) && elem nodeName input.toWhiteList) nodeList;
                to_nodes = node.to_node
                  ++ (attrNames (filterAttrs (name: other_node: elem nodeName other_node.from_node) input.nodes))
                  ++ (
                  if input.toBlackList != null then
                    filter (other_node: !elem other_node input.toBlackList) nodeList
                  else input.toWhiteList
                ) ++ optionals ((input.fromBlackList == null || !elem nodeName input.fromBlackList) && elem nodeName input.fromWhiteList) nodeList;
                peer_node = node.peer_node
                  ++ (attrNames (filterAttrs (name: other_node: elem nodeName other_node.peer_node) input.nodes))
                  ++ (
                  if input.peersBlackList != null then
                    if elem nodeName input.peersBlackList then
                      [ ]
                    else
                      filter (other_node: !elem other_node input.peersWhiteList) nodeList
                  else if elem nodeName input.peersWhiteList then
                    nodeList
                  else input.peersWhiteList
                );
              in
              {
                config = node.config;
                from = optionalAttrs directed
                  (listToAttrs (map
                    (other_node: {
                      name = other_node.name;
                      value = inut.defaultEdgeConfig;
                    })
                    from_nodes))
                // node.from
                // mapAttrs
                  (name: other_node: (getAttr nodeName other_node.to))
                  (filterAttrs
                    (name: other_node: (hasAttr nodeName other_node.to))
                    input.nodes);
                to = optionalAttrs directed
                  (listToAttrs (map
                    (other_node: {
                      name = other_node.name;
                      value = input.defaultEdge;
                    })
                    to_nodes))
                // node.to
                // mapAttrs
                  (name: other_node: (getAttr nodeName other_node.from))
                  (filterAttrs
                    (name: other_node: (hasAttr nodeName other_node.from))
                    input.nodes);
                peers = optionalAttrs (!directed)
                  ((listToAttrs (map
                    (name: {
                      name = name;
                      value = input.defaultEdge;
                    })
                    peer_node))
                  // node.peers
                  // mapAttrs
                    (name: other_node: (getAttr nodeName other_node.peers))
                    (filterAttrs
                      (name: other_node: (hasAttr nodeName other_node.peers))
                      input.nodes));
              })
            input.nodes;
        };
    in
    { inherit type function; };
  # lib.types.cluster = graph;
  # lib.types.clusters = args: attrsOf (submodule ({ config, name, ... }: { options = graph args; }));
}
