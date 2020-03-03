{Resources}
resource GB-RAM, DIMM-slot.

resource Intel-CPU-socket, AMD-CPU-socket, x86-processor.

resource monitor, VGA-port, HDMI-port.

resource OS.

resource DKK.

{RAM}
component 8GB-module:
  provides 8 GB-RAM;
  uses 1 DIMM-slot;
  uses 500 DKK.

component 4GB-module:
  provides 4 GB-RAM;
  uses 1 DIMM-slot;
  uses 300 DKK.

{Motherboards}
component Intel-motherboard:
  provides 2 DIMM-slot;
  provides Intel-CPU-socket;
  provides VGA-port; {integrated graphics}
  uses 500 DKK.

component AMD-motherboard:
  provides 2 DIMM-slot;
  provides AMD-CPU-socket;
  uses 600 DKK.

{Processors}
component Intel-i8:
  provides x86-processor;
  uses Intel-CPU-socket;
  uses 800 DKK.

component AMD-Ryzen6:
  provides x86-processor;
  uses AMD-CPU-socket;
  uses 700 DKK.
  
{Graphics cards and monitors}
component NVIDIA-GPU:
  provides 2 HDMI-port;
  uses 1500 DKK.

component monitor1:
  provides monitor;
  requires HDMI-port;
  uses 1200 DKK.

component monitor2:
  provides monitor;
  requires HDMI-port | VGA-port;
  uses 1500 DKK.

{Software}
component Linux:
  provides OS;
  requires x86-processor;
  uses 0 DKK. {Free!}

component Windows:
  provides OS;
  requires x86-processor;
  uses 1000 DKK.