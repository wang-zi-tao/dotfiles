{
  pkgs,
  config,
  lib,
  ...
}:
with builtins;
with lib;
let
  inherit (config.cluster) nodeConfig;
  nodeList = attrValues config.cluster.nodes;
  networkConfig = config.cluster.network.edges.${config.cluster.nodeName}.config;
  wireguardConfig = config.cluster.wireguard.edges.${config.cluster.nodeName}.config;
  grafanaConfig = ''
    {
      "annotations": {
        "list": [
          {
            "builtIn": 1,
            "datasource": {
              "type": "grafana",
              "uid": "-- Grafana --"
            },
            "enable": true,
            "hide": true,
            "iconColor": "rgba(0, 211, 255, 1)",
            "name": "Annotations & Alerts",
            "type": "dashboard"
          }
        ]
      },
      "editable": true,
      "fiscalYearStartMonth": 0,
      "graphTooltip": 0,
      "id": 1,
      "links": [],
      "liveNow": false,
      "panels": [
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "thresholds"
              },
              "mappings": [],
              "max": 100,
              "min": 0,
              "thresholds": {
                "mode": "absolute",
                "steps": [
                  {
                    "color": "green",
                    "value": null
                  },
                  {
                    "color": "red",
                    "value": 80
                  }
                ]
              },
              "unit": "%"
            },
            "overrides": []
          },
          "gridPos": {
            "h": 5,
            "w": 8,
            "x": 0,
            "y": 0
          },
          "id": 6,
          "options": {
            "colorMode": "value",
            "graphMode": "area",
            "justifyMode": "auto",
            "orientation": "auto",
            "reduceOptions": {
              "calcs": [
                "lastNotNull"
              ],
              "fields": "",
              "values": false
            },
            "textMode": "auto",
            "wideLayout": true
          },
          "pluginVersion": "10.2.4",
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "100 * (1 - sum by (instance)(increase(node_cpu_seconds_total{mode=\"idle\"}[5m])) / sum by (instance)(increase(node_cpu_seconds_total[5m])))",
              "legendFormat": "__auto",
              "range": true,
              "refId": "A"
            }
          ],
          "title": "CPU",
          "type": "stat"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "thresholds"
              },
              "mappings": [],
              "thresholds": {
                "mode": "absolute",
                "steps": [
                  {
                    "color": "green",
                    "value": null
                  },
                  {
                    "color": "red",
                    "value": 80
                  }
                ]
              },
              "unit": "decbytes"
            },
            "overrides": []
          },
          "gridPos": {
            "h": 5,
            "w": 8,
            "x": 8,
            "y": 0
          },
          "id": 7,
          "options": {
            "minVizHeight": 75,
            "minVizWidth": 75,
            "orientation": "auto",
            "reduceOptions": {
              "calcs": [
                "lastNotNull"
              ],
              "fields": "",
              "values": false
            },
            "showThresholdLabels": false,
            "showThresholdMarkers": true,
            "sizing": "auto"
          },
          "pluginVersion": "10.2.4",
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "sum(SeaweedFS_volumeServer_total_disk_size) by (job)",
              "legendFormat": "__auto",
              "range": true,
              "refId": "A"
            }
          ],
          "title": "Volume",
          "type": "gauge"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "palette-classic"
              },
              "custom": {
                "axisBorderShow": false,
                "axisCenteredZero": false,
                "axisColorMode": "text",
                "axisLabel": "",
                "axisPlacement": "auto",
                "barAlignment": 0,
                "drawStyle": "line",
                "fillOpacity": 0,
                "gradientMode": "none",
                "hideFrom": {
                  "legend": false,
                  "tooltip": false,
                  "viz": false
                },
                "insertNulls": false,
                "lineInterpolation": "linear",
                "lineWidth": 1,
                "pointSize": 5,
                "scaleDistribution": {
                  "type": "linear"
                },
                "showPoints": "auto",
                "spanNulls": false,
                "stacking": {
                  "group": "A",
                  "mode": "none"
                },
                "thresholdsStyle": {
                  "mode": "off"
                }
              },
              "mappings": [],
              "thresholds": {
                "mode": "absolute",
                "steps": [
                  {
                    "color": "green",
                    "value": null
                  },
                  {
                    "color": "red",
                    "value": 80
                  }
                ]
              },
              "unit": "%"
            },
            "overrides": []
          },
          "gridPos": {
            "h": 5,
            "w": 8,
            "x": 16,
            "y": 0
          },
          "id": 11,
          "options": {
            "legend": {
              "calcs": [],
              "displayMode": "list",
              "placement": "bottom",
              "showLegend": true
            },
            "tooltip": {
              "mode": "single",
              "sort": "none"
            }
          },
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "100 * (1 - sum by (instance)(increase(node_cpu_seconds_total{mode=\"idle\"}[5m])) / sum by (instance)(increase(node_cpu_seconds_total[5m])))",
              "legendFormat": "{{job}}",
              "range": true,
              "refId": "A"
            }
          ],
          "title": "CPU",
          "type": "timeseries"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "thresholds"
              },
              "mappings": [],
              "max": 100,
              "min": 0,
              "thresholds": {
                "mode": "absolute",
                "steps": [
                  {
                    "color": "green",
                    "value": null
                  },
                  {
                    "color": "red",
                    "value": 80
                  }
                ]
              },
              "unit": "percent"
            },
            "overrides": []
          },
          "gridPos": {
            "h": 5,
            "w": 8,
            "x": 0,
            "y": 5
          },
          "id": 5,
          "options": {
            "minVizHeight": 75,
            "minVizWidth": 75,
            "orientation": "auto",
            "reduceOptions": {
              "calcs": [
                "lastNotNull"
              ],
              "fields": "",
              "values": false
            },
            "showThresholdLabels": false,
            "showThresholdMarkers": true,
            "sizing": "auto"
          },
          "pluginVersion": "10.2.4",
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "node_memory_Active_bytes/node_memory_MemTotal_bytes*100",
              "legendFormat": "{{job}}",
              "range": true,
              "refId": "A"
            }
          ],
          "title": "Memory",
          "type": "gauge"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "palette-classic"
              },
              "custom": {
                "axisBorderShow": false,
                "axisCenteredZero": false,
                "axisColorMode": "text",
                "axisLabel": "",
                "axisPlacement": "auto",
                "barAlignment": 0,
                "drawStyle": "line",
                "fillOpacity": 0,
                "gradientMode": "none",
                "hideFrom": {
                  "legend": false,
                  "tooltip": false,
                  "viz": false
                },
                "insertNulls": false,
                "lineInterpolation": "linear",
                "lineWidth": 1,
                "pointSize": 5,
                "scaleDistribution": {
                  "type": "linear"
                },
                "showPoints": "auto",
                "spanNulls": false,
                "stacking": {
                  "group": "A",
                  "mode": "none"
                },
                "thresholdsStyle": {
                  "mode": "off"
                }
              },
              "mappings": [],
              "thresholds": {
                "mode": "absolute",
                "steps": [
                  {
                    "color": "green",
                    "value": null
                  },
                  {
                    "color": "red",
                    "value": 80
                  }
                ]
              },
              "unit": "bytes"
            },
            "overrides": []
          },
          "gridPos": {
            "h": 5,
            "w": 8,
            "x": 8,
            "y": 5
          },
          "id": 8,
          "options": {
            "legend": {
              "calcs": [],
              "displayMode": "list",
              "placement": "bottom",
              "showLegend": true
            },
            "tooltip": {
              "mode": "single",
              "sort": "none"
            }
          },
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "sum(rate(node_network_receive_bytes_total[5m]))by(job)",
              "legendFormat": "{{job}}_down",
              "range": true,
              "refId": "A"
            },
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "sum(rate(node_network_transmit_bytes_total[5m]))by(job)",
              "hide": false,
              "legendFormat": "{{job}}_up",
              "range": true,
              "refId": "B"
            }
          ],
          "title": "Network",
          "type": "timeseries"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "description": "",
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "palette-classic"
              },
              "custom": {
                "axisBorderShow": false,
                "axisCenteredZero": false,
                "axisColorMode": "text",
                "axisLabel": "",
                "axisPlacement": "auto",
                "barAlignment": 0,
                "drawStyle": "line",
                "fillOpacity": 0,
                "gradientMode": "none",
                "hideFrom": {
                  "legend": false,
                  "tooltip": false,
                  "viz": false
                },
                "insertNulls": false,
                "lineInterpolation": "linear",
                "lineWidth": 1,
                "pointSize": 5,
                "scaleDistribution": {
                  "type": "linear"
                },
                "showPoints": "auto",
                "spanNulls": false,
                "stacking": {
                  "group": "A",
                  "mode": "none"
                },
                "thresholdsStyle": {
                  "mode": "off"
                }
              },
              "mappings": [],
              "thresholds": {
                "mode": "absolute",
                "steps": [
                  {
                    "color": "green",
                    "value": null
                  },
                  {
                    "color": "red",
                    "value": 80
                  }
                ]
              }
            },
            "overrides": []
          },
          "gridPos": {
            "h": 5,
            "w": 8,
            "x": 16,
            "y": 5
          },
          "id": 12,
          "options": {
            "legend": {
              "calcs": [],
              "displayMode": "list",
              "placement": "bottom",
              "showLegend": true
            },
            "tooltip": {
              "mode": "single",
              "sort": "none"
            }
          },
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "sum(rate(node_context_switches_total[5m]))by(job)",
              "legendFormat": "__auto",
              "range": true,
              "refId": "A"
            }
          ],
          "title": "context switch",
          "type": "timeseries"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "palette-classic"
              },
              "custom": {
                "axisBorderShow": false,
                "axisCenteredZero": false,
                "axisColorMode": "text",
                "axisLabel": "",
                "axisPlacement": "auto",
                "barAlignment": 0,
                "drawStyle": "line",
                "fillOpacity": 0,
                "gradientMode": "none",
                "hideFrom": {
                  "legend": false,
                  "tooltip": false,
                  "viz": false
                },
                "insertNulls": false,
                "lineInterpolation": "linear",
                "lineWidth": 1,
                "pointSize": 5,
                "scaleDistribution": {
                  "type": "linear"
                },
                "showPoints": "auto",
                "spanNulls": false,
                "stacking": {
                  "group": "A",
                  "mode": "none"
                },
                "thresholdsStyle": {
                  "mode": "off"
                }
              },
              "mappings": [],
              "thresholds": {
                "mode": "absolute",
                "steps": [
                  {
                    "color": "green",
                    "value": null
                  },
                  {
                    "color": "red",
                    "value": 80
                  }
                ]
              },
              "unit": "%"
            },
            "overrides": []
          },
          "gridPos": {
            "h": 7,
            "w": 4,
            "x": 0,
            "y": 10
          },
          "id": 1,
          "options": {
            "legend": {
              "calcs": [],
              "displayMode": "list",
              "placement": "bottom",
              "showLegend": true
            },
            "tooltip": {
              "mode": "single",
              "sort": "none"
            }
          },
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "100 * (1 - sum by (instance)(increase(node_cpu_seconds_total{mode=\"idle\"}[5m])) / sum by (instance)(increase(node_cpu_seconds_total[5m])))",
              "legendFormat": "__auto",
              "range": true,
              "refId": "A"
            }
          ],
          "title": "CPU",
          "type": "timeseries"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "palette-classic"
              },
              "custom": {
                "axisBorderShow": false,
                "axisCenteredZero": false,
                "axisColorMode": "text",
                "axisLabel": "",
                "axisPlacement": "auto",
                "barAlignment": 0,
                "drawStyle": "line",
                "fillOpacity": 0,
                "gradientMode": "none",
                "hideFrom": {
                  "legend": false,
                  "tooltip": false,
                  "viz": false
                },
                "insertNulls": false,
                "lineInterpolation": "linear",
                "lineWidth": 1,
                "pointSize": 5,
                "scaleDistribution": {
                  "type": "linear"
                },
                "showPoints": "auto",
                "spanNulls": false,
                "stacking": {
                  "group": "A",
                  "mode": "none"
                },
                "thresholdsStyle": {
                  "mode": "off"
                }
              },
              "mappings": [],
              "thresholds": {
                "mode": "absolute",
                "steps": [
                  {
                    "color": "green",
                    "value": null
                  },
                  {
                    "color": "red",
                    "value": 80
                  }
                ]
              },
              "unit": "%"
            },
            "overrides": []
          },
          "gridPos": {
            "h": 7,
            "w": 4,
            "x": 4,
            "y": 10
          },
          "id": 3,
          "options": {
            "legend": {
              "calcs": [],
              "displayMode": "list",
              "placement": "bottom",
              "showLegend": true
            },
            "tooltip": {
              "mode": "single",
              "sort": "none"
            }
          },
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "exemplar": false,
              "expr": "node_memory_Active_bytes/node_memory_MemTotal_bytes*100",
              "instant": false,
              "legendFormat": "{{job}}",
              "range": true,
              "refId": "A"
            }
          ],
          "title": "Memory",
          "type": "timeseries"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "palette-classic"
              },
              "custom": {
                "axisBorderShow": false,
                "axisCenteredZero": false,
                "axisColorMode": "text",
                "axisLabel": "",
                "axisPlacement": "auto",
                "barAlignment": 0,
                "drawStyle": "line",
                "fillOpacity": 0,
                "gradientMode": "none",
                "hideFrom": {
                  "legend": false,
                  "tooltip": false,
                  "viz": false
                },
                "insertNulls": false,
                "lineInterpolation": "linear",
                "lineWidth": 1,
                "pointSize": 5,
                "scaleDistribution": {
                  "type": "linear"
                },
                "showPoints": "auto",
                "spanNulls": false,
                "stacking": {
                  "group": "A",
                  "mode": "none"
                },
                "thresholdsStyle": {
                  "mode": "off"
                }
              },
              "mappings": [],
              "thresholds": {
                "mode": "absolute",
                "steps": [
                  {
                    "color": "green",
                    "value": null
                  },
                  {
                    "color": "red",
                    "value": 80
                  }
                ]
              }
            },
            "overrides": []
          },
          "gridPos": {
            "h": 7,
            "w": 4,
            "x": 8,
            "y": 10
          },
          "id": 2,
          "options": {
            "legend": {
              "calcs": [],
              "displayMode": "list",
              "placement": "bottom",
              "showLegend": true
            },
            "tooltip": {
              "mode": "single",
              "sort": "none"
            }
          },
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "avg by (job) (rate(SeaweedFS_filerStore_request_seconds_count[5m]))",
              "legendFormat": "__auto",
              "range": true,
              "refId": "A"
            }
          ],
          "title": "Seaweedfs filer",
          "type": "timeseries"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "palette-classic"
              },
              "custom": {
                "axisBorderShow": false,
                "axisCenteredZero": false,
                "axisColorMode": "text",
                "axisLabel": "",
                "axisPlacement": "auto",
                "barAlignment": 0,
                "drawStyle": "line",
                "fillOpacity": 0,
                "gradientMode": "none",
                "hideFrom": {
                  "legend": false,
                  "tooltip": false,
                  "viz": false
                },
                "insertNulls": false,
                "lineInterpolation": "linear",
                "lineWidth": 1,
                "pointSize": 5,
                "scaleDistribution": {
                  "type": "linear"
                },
                "showPoints": "auto",
                "spanNulls": false,
                "stacking": {
                  "group": "A",
                  "mode": "none"
                },
                "thresholdsStyle": {
                  "mode": "off"
                }
              },
              "mappings": [],
              "thresholds": {
                "mode": "absolute",
                "steps": [
                  {
                    "color": "green",
                    "value": null
                  },
                  {
                    "color": "red",
                    "value": 80
                  }
                ]
              }
            },
            "overrides": []
          },
          "gridPos": {
            "h": 7,
            "w": 4,
            "x": 12,
            "y": 10
          },
          "id": 4,
          "options": {
            "legend": {
              "calcs": [],
              "displayMode": "list",
              "placement": "bottom",
              "showLegend": true
            },
            "tooltip": {
              "mode": "single",
              "sort": "none"
            }
          },
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": " avg by (job) (rate(SeaweedFS_volumeServer_request_seconds_count[5m]))",
              "legendFormat": "__auto",
              "range": true,
              "refId": "A"
            }
          ],
          "title": "Seaweedfs Volume",
          "type": "timeseries"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "palette-classic"
              },
              "custom": {
                "axisBorderShow": false,
                "axisCenteredZero": false,
                "axisColorMode": "text",
                "axisLabel": "",
                "axisPlacement": "auto",
                "barAlignment": 0,
                "drawStyle": "line",
                "fillOpacity": 0,
                "gradientMode": "none",
                "hideFrom": {
                  "legend": false,
                  "tooltip": false,
                  "viz": false
                },
                "insertNulls": false,
                "lineInterpolation": "linear",
                "lineWidth": 1,
                "pointSize": 5,
                "scaleDistribution": {
                  "type": "linear"
                },
                "showPoints": "auto",
                "spanNulls": false,
                "stacking": {
                  "group": "A",
                  "mode": "none"
                },
                "thresholdsStyle": {
                  "mode": "off"
                }
              },
              "mappings": [],
              "thresholds": {
                "mode": "absolute",
                "steps": [
                  {
                    "color": "green",
                    "value": null
                  },
                  {
                    "color": "red",
                    "value": 80
                  }
                ]
              }
            },
            "overrides": []
          },
          "gridPos": {
            "h": 5,
            "w": 8,
            "x": 16,
            "y": 10
          },
          "id": 13,
          "options": {
            "legend": {
              "calcs": [],
              "displayMode": "list",
              "placement": "bottom",
              "showLegend": true
            },
            "tooltip": {
              "mode": "single",
              "sort": "none"
            }
          },
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "node_load15",
              "legendFormat": "{{job}}",
              "range": true,
              "refId": "A"
            }
          ],
          "title": "load15",
          "type": "timeseries"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "palette-classic"
              },
              "custom": {
                "axisBorderShow": false,
                "axisCenteredZero": false,
                "axisColorMode": "text",
                "axisLabel": "",
                "axisPlacement": "auto",
                "barAlignment": 0,
                "drawStyle": "line",
                "fillOpacity": 0,
                "gradientMode": "none",
                "hideFrom": {
                  "legend": false,
                  "tooltip": false,
                  "viz": false
                },
                "insertNulls": false,
                "lineInterpolation": "linear",
                "lineWidth": 1,
                "pointSize": 5,
                "scaleDistribution": {
                  "type": "linear"
                },
                "showPoints": "auto",
                "spanNulls": false,
                "stacking": {
                  "group": "A",
                  "mode": "none"
                },
                "thresholdsStyle": {
                  "mode": "off"
                }
              },
              "mappings": [],
              "thresholds": {
                "mode": "absolute",
                "steps": [
                  {
                    "color": "green",
                    "value": null
                  },
                  {
                    "color": "red",
                    "value": 80
                  }
                ]
              }
            },
            "overrides": []
          },
          "gridPos": {
            "h": 7,
            "w": 8,
            "x": 16,
            "y": 15
          },
          "id": 14,
          "options": {
            "legend": {
              "calcs": [],
              "displayMode": "list",
              "placement": "bottom",
              "showLegend": true
            },
            "tooltip": {
              "mode": "single",
              "sort": "none"
            }
          },
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "node_load1",
              "legendFormat": "{{job}}",
              "range": true,
              "refId": "A"
            }
          ],
          "title": "load1",
          "type": "timeseries"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "palette-classic"
              },
              "custom": {
                "axisBorderShow": false,
                "axisCenteredZero": false,
                "axisColorMode": "text",
                "axisLabel": "",
                "axisPlacement": "auto",
                "barAlignment": 0,
                "drawStyle": "line",
                "fillOpacity": 0,
                "gradientMode": "none",
                "hideFrom": {
                  "legend": false,
                  "tooltip": false,
                  "viz": false
                },
                "insertNulls": false,
                "lineInterpolation": "linear",
                "lineWidth": 1,
                "pointSize": 5,
                "scaleDistribution": {
                  "type": "linear"
                },
                "showPoints": "auto",
                "spanNulls": false,
                "stacking": {
                  "group": "A",
                  "mode": "none"
                },
                "thresholdsStyle": {
                  "mode": "off"
                }
              },
              "mappings": [],
              "thresholds": {
                "mode": "absolute",
                "steps": [
                  {
                    "color": "green",
                    "value": null
                  },
                  {
                    "color": "red",
                    "value": 80
                  }
                ]
              },
              "unit": "bytes"
            },
            "overrides": []
          },
          "gridPos": {
            "h": 5,
            "w": 8,
            "x": 0,
            "y": 17
          },
          "id": 9,
          "options": {
            "legend": {
              "calcs": [],
              "displayMode": "list",
              "placement": "bottom",
              "showLegend": true
            },
            "tooltip": {
              "mode": "single",
              "sort": "none"
            }
          },
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "sum(rate(node_disk_read_bytes_total[5m]))by(job)",
              "legendFormat": "{{job}}_read",
              "range": true,
              "refId": "A"
            },
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "sum(rate(node_disk_written_bytes_total[5m]))by(job)",
              "hide": false,
              "legendFormat": "{{job}}_write",
              "range": true,
              "refId": "B"
            }
          ],
          "title": "Disk IO",
          "type": "timeseries"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "palette-classic"
              },
              "custom": {
                "axisBorderShow": false,
                "axisCenteredZero": false,
                "axisColorMode": "text",
                "axisLabel": "",
                "axisPlacement": "auto",
                "barAlignment": 0,
                "drawStyle": "line",
                "fillOpacity": 0,
                "gradientMode": "none",
                "hideFrom": {
                  "legend": false,
                  "tooltip": false,
                  "viz": false
                },
                "insertNulls": false,
                "lineInterpolation": "linear",
                "lineWidth": 1,
                "pointSize": 5,
                "scaleDistribution": {
                  "type": "linear"
                },
                "showPoints": "auto",
                "spanNulls": false,
                "stacking": {
                  "group": "A",
                  "mode": "none"
                },
                "thresholdsStyle": {
                  "mode": "off"
                }
              },
              "mappings": [],
              "thresholds": {
                "mode": "absolute",
                "steps": [
                  {
                    "color": "green",
                    "value": null
                  },
                  {
                    "color": "red",
                    "value": 80
                  }
                ]
              }
            },
            "overrides": []
          },
          "gridPos": {
            "h": 5,
            "w": 8,
            "x": 8,
            "y": 17
          },
          "id": 10,
          "options": {
            "legend": {
              "calcs": [],
              "displayMode": "list",
              "placement": "bottom",
              "showLegend": true
            },
            "tooltip": {
              "mode": "single",
              "sort": "none"
            }
          },
          "pluginVersion": "9.5.6",
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "sum(rate(node_context_switches_total[5m]))by(job)/300",
              "legendFormat": "{{job}}",
              "range": true,
              "refId": "A"
            }
          ],
          "title": "Context Switch",
          "type": "timeseries"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "palette-classic"
              },
              "custom": {
                "hideFrom": {
                  "legend": false,
                  "tooltip": false,
                  "viz": false
                }
              },
              "mappings": [],
              "unit": "decbytes"
            },
            "overrides": []
          },
          "gridPos": {
            "h": 18,
            "w": 5,
            "x": 0,
            "y": 22
          },
          "id": 15,
          "options": {
            "legend": {
              "displayMode": "list",
              "placement": "bottom",
              "showLegend": true
            },
            "pieType": "pie",
            "reduceOptions": {
              "calcs": [
                "lastNotNull"
              ],
              "fields": "",
              "values": false
            },
            "tooltip": {
              "mode": "single",
              "sort": "none"
            }
          },
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "expr": "sum(namedprocess_namegroup_memory_bytes) by(groupname)",
              "instant": false,
              "legendFormat": "__auto",
              "range": true,
              "refId": "A"
            }
          ],
          "title": "Process Virtual Memory",
          "type": "piechart"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "palette-classic"
              },
              "custom": {
                "hideFrom": {
                  "legend": false,
                  "tooltip": false,
                  "viz": false
                }
              },
              "mappings": [],
              "unit": "percentunit"
            },
            "overrides": []
          },
          "gridPos": {
            "h": 18,
            "w": 5,
            "x": 5,
            "y": 22
          },
          "id": 16,
          "options": {
            "legend": {
              "displayMode": "list",
              "placement": "bottom",
              "showLegend": true
            },
            "pieType": "pie",
            "reduceOptions": {
              "calcs": [
                "lastNotNull"
              ],
              "fields": "",
              "values": false
            },
            "tooltip": {
              "mode": "single",
              "sort": "none"
            }
          },
          "pluginVersion": "10.2.4",
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "exemplar": false,
              "expr": "sum(increase(namedprocess_namegroup_cpu_seconds_total[5m]))by(groupname) / 300",
              "instant": true,
              "legendFormat": "__auto",
              "range": false,
              "refId": "A"
            }
          ],
          "title": "Process Cpu Usage",
          "type": "piechart"
        },
        {
          "datasource": {
            "type": "prometheus",
            "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
          },
          "fieldConfig": {
            "defaults": {
              "color": {
                "mode": "palette-classic"
              },
              "custom": {
                "hideFrom": {
                  "legend": false,
                  "tooltip": false,
                  "viz": false
                }
              },
              "mappings": []
            },
            "overrides": []
          },
          "gridPos": {
            "h": 18,
            "w": 5,
            "x": 10,
            "y": 22
          },
          "id": 17,
          "options": {
            "legend": {
              "displayMode": "list",
              "placement": "bottom",
              "showLegend": true
            },
            "pieType": "pie",
            "reduceOptions": {
              "calcs": [
                "lastNotNull"
              ],
              "fields": "",
              "values": false
            },
            "tooltip": {
              "mode": "single",
              "sort": "none"
            }
          },
          "targets": [
            {
              "datasource": {
                "type": "prometheus",
                "uid": "d35509bd-d00e-4192-a5de-d24e61dd41a0"
              },
              "editorMode": "code",
              "exemplar": false,
              "expr": "sum(increase(namedprocess_namegroup_context_switches_total[5m]))by(groupname)/300",
              "instant": true,
              "legendFormat": "__auto",
              "range": false,
              "refId": "A"
            }
          ],
          "title": "Process Context Switch",
          "type": "piechart"
        }
      ],
      "refresh": "10s",
      "schemaVersion": 39,
      "tags": [],
      "templating": {
        "list": []
      },
      "time": {
        "from": "now-5m",
        "to": "now"
      },
      "timepicker": {},
      "timezone": "",
      "title": "New dashboard",
      "uid": "ec7f6afd-deec-4175-bf16-fc7e5191b3b9",
      "version": 19,
      "weekStart": ""
    }
  '';
in
{
  imports = [ ./wireguard.nix ];
  config = {
    sops.secrets = optionalAttrs nodeConfig.prometheus.server { "prometheus/admin_password" = { }; };
    services.grafana = mkIf nodeConfig.prometheus.server {
      enable = true;
      settings = {
        server = {
          root_url = "%(protocol)s://%(domain)s:%(http_port)s/prometheus";
          http_addr = "0.0.0.0";
          http_port = 9002;
          domain = "grafana.wangzicloud.cn";
        };
        security.adminPasswordFile = config.sops.secrets."prometheus/admin_password".path;
      };
    };
    services.caddy = lib.optionalAttrs nodeConfig.prometheus.server {
      enable = true;
      virtualHosts = {
        "https://${builtins.toString networkConfig.publicIp}" = {
          extraConfig = ''
            route /prometheus/* {
                uri strip_prefix /prometheus
                reverse_proxy http://localhost:9002
            }
          '';
        };
      };
    };
    services.prometheus = {
      enable = nodeConfig.prometheus.server;
      port = 9001;
      scrapeConfigs =
        (map (node: {
          job_name = "${node.hostname}";
          static_configs = [
            {
              targets = [
                "${node.hostname}.wg:9100"
                "${node.hostname}.wg:9256"
              ];
            }
          ];
        }) (filter (node: node.prometheus.nodeExporter) nodeList))
        ++ (map (node: {
          job_name = "${node.hostname}-wg";
          static_configs = [ { targets = [ "${node.hostname}.wg:9586" ]; } ];
        }) (filter (node: hasAttr node.hostname config.cluster.wireguard.edges) nodeList))
        ++ (map (node: {
          job_name = "${node.hostname}-weed";
          metrics_path = "/metrics";
          static_configs = [ { targets = [ "${node.hostname}.wg:9101" ]; } ];
        }) (filter (node: hasAttr node.hostname config.cluster.seaweedfs.edges) nodeList));
      exporters = {
        node = mkIf nodeConfig.prometheus.nodeExporter {
          enable = true;
          enabledCollectors = [ "systemd" ];
          listenAddress = wireguardConfig.clusterIp;
          port = 9100;
        };
        wireguard = mkIf nodeConfig.prometheus.nodeExporter {
          enable = true;
          listenAddress = wireguardConfig.clusterIp;
          verbose = true;
          port = 9586;
        };
        process = mkIf nodeConfig.prometheus.nodeExporter {
          enable = true;
          listenAddress = wireguardConfig.clusterIp;
          port = 9256;
          settings.process_names = [
            {
              name = "{{.Matches.Wrapped}}";
              cmdline = [ "^/nix/store[^ ]*/(?P<Wrapped>[^ /]*) (?P<Args>.*)" ];
            }
            {
              name = "{{.Matches.Wrapped}}";
              cmdline = [ "^(?P<Wrapped>[^ /]*) (?P<Args>.*)" ];
            }
          ];
        };
      };
    };
    networking.firewall.allowedUDPPorts = [ 9003 ];
    networking.firewall.allowedTCPPorts = [ 9003 ];
  };
}
