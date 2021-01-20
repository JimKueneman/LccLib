program project_lcc_node;

{$mode objfpc}{$H+}

{ Raspberry Pi 2 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi2,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
   { Add additional units here }
  lcc_utilities,
  lcc_defines,
  lcc_node,
  lcc_node_manager,
  lcc_node_messages,
  lcc_protocol_acdi,
  lcc_protocol_base,
  lcc_protocol_datagram,
  lcc_protocol_events,
  lcc_protocol_memory_configuration,
  lcc_protocol_memory_configurationdefinitioninfo,
  lcc_protocol_memory_information,
  lcc_protocol_memory_options,
  lcc_protocol_simplenodeinfo,
  lcc_protocol_supportedprotocols,
  lcc_ethernet_server,
  lcc_common_classes,
  lcc_ethernet_common;

begin
  { Add your program code here }
end.

