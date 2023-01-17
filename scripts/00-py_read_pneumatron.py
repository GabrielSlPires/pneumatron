import serial.tools.list_ports
from datetime import datetime
import time

def com_ports():
    ports_name = serial.tools.list_ports.comports()
    ports = []

    for port, desc, hwid in sorted(ports_name):
        ports.append(port)

    return(ports)

ports = com_ports()
ser = {port: serial.Serial(port, 115200, timeout=0) for port in ports}

while True:
    for port in ports:
        measures = ser[port].readlines()
        for measure in measures:
            now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            with open('data/raw_pneumatron/pneumatron_database.csv', 'a') as f:
                #separar por virgular??
                line = f'{measure.decode("utf-8")[:-1]},{now}\n'
                #filtrar ,, e , na primeira posicao
                if line.count(',') == 5:
                    f.write(line)
                f.close()
    if com_ports() != ports:
        print("Update COM Ports!")
        ports = com_ports()
        ser = {port: serial.Serial(port, 115200) for port in ports}


