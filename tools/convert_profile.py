import json
import os
import struct
import sys

kernel_path = "kernel/kernel"

src = open(sys.argv[1], "rb")
dst = open(sys.argv[2], "w")

# Load symbols from kernel using llvm-readelf's json output
readelfOut = json.load(os.popen(f"llvm-readelf -s --demangle --elf-output-style=JSON {kernel_path}"))
mapAddrToName = {}
for symbol in readelfOut[0]["Symbols"]:
	mapAddrToName[symbol["Symbol"]["Value"]] = symbol["Symbol"]["Name"]["Name"]

# Derive the kernel base address from kernel_entry.
kernel_load_offset = [addr for addr, name in mapAddrToName.items() if name == "kernel_entry"]
assert len(kernel_load_offset) == 1
kernel_load_offset = kernel_load_offset[0]

# TODO: Replace the hardcoded offset with some automation
src.seek(0x175)

# Format of each event. See profiling.cpp for struct ProfileEvent.
event_format = "<QL"
event_size = struct.calcsize(event_format)

dst.write("[")

while event_buf := src.read(event_size):
	(tscval, func_offset) = struct.unpack("<QL", event_buf)
	event_json = {
		"name": mapAddrToName[kernel_load_offset + func_offset],
		"ph": "E" if (tscval & 1) else "B",
		"ts": float(tscval & ~1) / 1000,
		"cat": "",
		"pid": 0, "tid": 0,
		"args": {}
	}
	dst.write(json.dumps(event_json))
	dst.write(",")

# No "]" needed. Also means we don't care about a trailing ",".
