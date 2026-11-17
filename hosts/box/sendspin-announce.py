"""Announce a sendspin board over mDNS on its behalf, while it is reachable.

Music Assistant connects to a board when its mDNS record appears. A board
behind the WireGuard tunnel can't announce itself here, so this does it:
the record is published once the board accepts connections, and withdrawn
when it stops answering, so that it appears again when the board is back.

Usage: sendspin-announce NAME ADDRESS INTERFACE_ADDRESS
"""

import signal
import socket
import struct
import sys
import time

from zeroconf import ServiceInfo, Zeroconf

SERVICE = "_sendspin._tcp.local."
# The port an ESPHome sendspin board listens on.
PORT = 8928
INTERVAL_S = 5
TIMEOUT_S = 3
# Consecutive missed pings before the record is withdrawn.
MISSES = 3


def accepts(address):
    """Whether the board's sendspin port accepts a connection."""
    try:
        with socket.create_connection((address, PORT), timeout=TIMEOUT_S):
            return True
    except OSError:
        return False


def pings(address):
    """Whether the board answers a ping, which disturbs it less."""
    # An unprivileged ICMP socket; the kernel fills in the id and checksum.
    with socket.socket(
        socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_ICMP
    ) as sock:
        sock.settimeout(TIMEOUT_S)
        try:
            sock.sendto(struct.pack("!BBHHH", 8, 0, 0, 0, 1), (address, 0))
            sock.recv(64)
            return True
        except OSError:
            return False


def stop(*_):
    raise SystemExit(0)


def main():
    name, address, interface = sys.argv[1:]
    info = ServiceInfo(
        SERVICE,
        f"{name}.{SERVICE}",
        addresses=[socket.inet_aton(address)],
        port=PORT,
        properties={"path": "/sendspin"},
        server=f"{name}.local.",
    )
    signal.signal(signal.SIGTERM, stop)
    zeroconf = Zeroconf(interfaces=[interface])
    announced = False
    misses = 0
    try:
        while True:
            if not announced:
                if accepts(address):
                    zeroconf.register_service(info)
                    announced = True
                    misses = 0
                    print(f"{name} is up at {address}; announced", flush=True)
            elif pings(address):
                misses = 0
            else:
                misses += 1
                if misses >= MISSES:
                    zeroconf.unregister_service(info)
                    announced = False
                    print(f"{name} stopped answering; withdrawn", flush=True)
            time.sleep(INTERVAL_S)
    finally:
        # Withdraws the record, if it is still announced.
        zeroconf.close()


main()
