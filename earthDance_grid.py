#! /usr/bin/env python3

import asyncio
import monome
import math
from pythonosc import udp_client

class SoundOnOff():
    def __init__(self, y, x, route):
        self.y = y
        self.x = x
        self.route = route
        self.basicBrightness = 4
        self.onOff = 0

    def draw(self):
        return self.onOff * 11 + self.basicBrightness;

    def toggleButton(self):
        self.onOff ^= 1

    def sendMessage(self):
        client.send_message("/ctrl", [self.route, self.onOff])

class EvalLine():
    def __init__(self, y, x, type, row):
        self.y = y
        self.x = x
        self.type = type
        self.basicBrightness = 4
        self.pressed = 0
        self.row = row

    def draw(self):
        return self.pressed* 11 + self.basicBrightness;

    def sendMessage(self):
        client2.send_message("/atom/eval", ["type", self.type, "row", self.row, "column", 1])

class Fader():
    def __init__(self, x, route):
        self.y = [0,0,0,0,0,0,0,0]
        self.x = x
        self.route = route
        self.basicBrightness = 4
        self.vol = 0
        self.destination = 0

    def draw(self, i):
        return self.y[i] * 11 + self.basicBrightness;

    def toggleButton(self, yPos):
        for index, row in enumerate(self.y):
            self.y[index] = 0
            if index == yPos:
                self.y[index] = 1
        self.destination = math.sqrt(1.0-(float(yPos)/7.0))

    def fadeVolume(self):
        self.vol += (self.destination-self.vol)*.75;
        self.sendMessage();

    def sendMessage(self):
        client.send_message("/ctrl", [self.route, self.vol])

class Value():
    def __init__(self, x, route, startVal, scale):
        self.x = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
        self.x[startVal-1] = 1
        self.y = x
        self.route = route
        self.basicBrightness = 4
        self.value = startVal/scale
        self.scale = scale
        self.sendMessage()

    def draw(self, i):
        return self.x[i] * 11 + self.basicBrightness;

    def toggleButton(self, xPos):
        for index, column in enumerate(self.x):
            self.x[index] = 0
            if index == xPos:
                self.x[index] = 1
                self.value = (xPos + 1) * self.scale
        print(self.value)
        self.sendMessage()

    def sendMessage(self):
        client.send_message("/ctrl", [self.route, self.value])

class MirroringMedusa(monome.GridApp):
    def __init__(self):
        super().__init__()


    def on_grid_ready(self):
        self.toggleSounds = []

        # self.toggleSounds.append(SoundOnOff(7, 0, "tablaVol"))
        # self.toggleSounds.append(SoundOnOff(7, 1, "aaronVol"))
        # self.toggleSounds.append(SoundOnOff(7, 2, "beat0"))
        # self.toggleSounds.append(SoundOnOff(7, 3, "soph0"))
        # self.toggleSounds.append(SoundOnOff(7, 4, "hh0"))
        # self.toggleSounds.append(SoundOnOff(7, 5, "arps"))


        self.values = []

        # row, route, start value, scale amount
        self.values.append(Value(0, "euc", 3, 1))
        self.values.append(Value(1, "div", 8, 1))
        self.values.append(Value(2, "shiftAmount", 1, 1/16))
        self.values.append(Value(3, "degradeIt", 16, 1/16))

        self.values.append(Value(5, "degradeIt2", 16, 1/16))
        self.values.append(Value(6, "segments", 10, 1))
        self.values.append(Value(7, "choosePat", 1, 1))


        asyncio.ensure_future(self.play())

        self.evals = []

        self.evals.append(EvalLine(4,0,"line",29))
        self.evals.append(EvalLine(4,1,"line",31))

        self.onOff = 0

    async def play(self):
        while True:
            self.draw()

            # for fader in self.faders:
            #     fader.fadeVolume()

            await asyncio.sleep(0.1)

    def draw(self):
        buffer = monome.GridBuffer(self.grid.width, self.grid.height)

        # all off
        # buffer.led_level_set(4, 0, self.onOff*11+4)

        for toggle in self.toggleSounds:
                buffer.led_level_set(toggle.x, toggle.y, toggle.draw())

        # for fader in self.faders:
        #     for index, row in enumerate(fader.y):
        #         buffer.led_level_set(fader.x, index, fader.draw(index))

        for v in self.values:
            for index, column in enumerate(v.x):
                buffer.led_level_set(index, v.y, v.draw(index))

        for e in self.evals:
            buffer.led_level_set(e.x, e.y, e.draw())
            e.pressed=0

        # update grid
        buffer.render(self.grid)

    def on_grid_key(self, x, y, s):
        # all off
        # if x == 4 and y ==0:
        #     self.onOff ^= 1
        #     for toggle in self.toggleSounds:
        #         toggle.onOff = 0
        #         toggle.sendMessage()
        #     for fader in self.faders:
        #         for index, row in enumerate(fader.y):
        #             fader.y[index] = 0
        #         fader.destination = 0
        # toggle steps
        if s == 1:
            for toggle in self.toggleSounds:
                if x == toggle.x and y == toggle.y:
                    toggle.onOff ^= 1;
                    toggle.sendMessage();

            # for fader in self.faders:
            #     if x == fader.x:
            #         fader.toggleButton(y)

            for v in self.values:
                if y == v.y:
                    v.toggleButton(x)

            for e in self.evals:
                    if e.x == x:
                        e.pressed = 1
                        e.sendMessage()

            self.draw()


if __name__ == '__main__':
    loop = asyncio.get_event_loop()
    mirroring_medusa = MirroringMedusa()

    def serialosc_device_added(id, type, port):
        print('connecting to {} ({})'.format(id, type))
        asyncio.ensure_future(mirroring_medusa.grid.connect('127.0.0.1', port))

    serialosc = monome.SerialOsc()
    serialosc.device_added_event.add_handler(serialosc_device_added)

    client = udp_client.SimpleUDPClient("127.0.0.1", 6010)
    client2 = udp_client.SimpleUDPClient("127.0.0.1", 3333)

    loop.run_until_complete(serialosc.connect())
    loop.run_forever()
