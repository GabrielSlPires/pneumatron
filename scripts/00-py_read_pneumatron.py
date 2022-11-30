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
ser = {port: serial.Serial(port, 115200) for port in ports}

while TRUE:
    for port in ports:
        buffer_size = ser[port].inWaiting()
        measures = ser[port].read(buffer_size)
        for measure in measures:
            now = datetime.now().strftime("%d-%m-%Y %H:%M:%S")
            with open('py_data.txt', 'a') as f:
                #separar por virgular??
                line = f'{measure.decode("utf-8")[:-1]},{now}'
                f.write(line)
                f.close()
        #aumentar tempo de espera para 15min?
    time.sleep(5)
    if com_ports() != ports:
        ports = com_ports()
        ser = {port: serial.Serial(port, 115200) for port in ports}


