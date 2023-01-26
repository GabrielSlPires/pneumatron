import serial.tools.list_ports
from datetime import datetime
import time
import os
import sys

file = "pneumatron_database-v4"

def com_ports():
    ports_name = serial.tools.list_ports.comports()
    ports = []

    for port, desc, hwid in sorted(ports_name):
        ports.append(port)

    return(ports)

ports = com_ports()
print(f"\nReading ports: {ports}\n")
ser = {port: serial.Serial(port, 115200, timeout=0.01) for port in ports}

try:
 while True:
     print("\rWaiting       ", end="")
     for port in ports:
         message = ser[port].readline()
         if len(message) > 2:
             measure = message.decode()
             now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
             try:
              with open(f'data/raw_pneumatron/{file}.csv', 'a') as f:
                  line = f'{measure[:-1]},{now}\n'
                  f.write(line)
                  f.close()
              print("\rReceiving data", end="")
             except Exception:
              pass
     new_com_port = not any([ser[port].inWaiting() for port in ports] + [ports == com_ports()])
     if new_com_port:
         print("\nNew COM Port - Restart script")
         time.sleep(1)
         os.execv(sys.executable,["python3"] + [sys.argv[0]])
     
except Exception:
 print("\nError found - Restart script")
 time.sleep(1)
 os.execv(sys.executable,["python3"] + [sys.argv[0]])
