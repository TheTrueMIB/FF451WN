import websocket  # type: ignore
import json
import csv
from datetime import datetime

csv_file = 'btc_usdt_live_prices.csv'

# Write header once (ADD columns)
with open(csv_file, mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow([
        'Timestamp',
        'Price',
        'Volume24h',
        'High24h',
        'Low24h',
        'Open24h'
    ])

max_messages = 200
message_count = 0

def on_message(ws, message):
    global message_count
    data = json.loads(message)

    if 'data' in data:
        ticker = data['data'][0]

        # EXISTING
        last_price = ticker['last']
        timestamp = datetime.utcnow().strftime('%Y-%m-%d %H:%M:%S')

        # NEW (from OKX ticker)
        vol24h = ticker.get('vol24h', None)
        high24h = ticker.get('high24h', None)
        low24h = ticker.get('low24h', None)
        open24h = ticker.get('open24h', None)

        print(f'{timestamp} | Price: {last_price} | Vol24h: {vol24h}')

        with open(csv_file, mode='a', newline='') as file:
            writer = csv.writer(file)
            writer.writerow([
                timestamp,
                last_price,
                vol24h,
                high24h,
                low24h,
                open24h
            ])

        message_count += 1

        if message_count >= max_messages:
            print("Reached max messages. Closing connection...")
            ws.close()

def on_open(ws):
    print("WebSocket connection opened.")
    subscribe_message = {
        "op": "subscribe",
        "args": [
            {
                "channel": "tickers",
                "instId": "BTC-USDT"
            }
        ]
    }
    ws.send(json.dumps(subscribe_message))

def on_error(ws, error):
    print(f"WebSocket error: {error}")

def on_close(ws, close_status_code, close_msg):
    print("WebSocket connection closed.")

socket = "wss://wspap.okx.com:8443/ws/v5/public"

ws = websocket.WebSocketApp(
    socket,
    on_open=on_open,
    on_message=on_message,
    on_error=on_error,
    on_close=on_close
)

ws.run_forever()



# This code connects to the OKX WebSocket API to receive live price updates for the BTC/USDT trading pair. It writes the timestamp and price to a CSV file, and it will stop after receiving 100 messages.
# run these in terminal:
# cd "C:\Users\Jackw\OneDrive\桌面\451 Bayesian\final project"
#py "BTC CSV read.py"
#or py "C:\Users\Jackw\OneDrive\桌面\451 Bayesian\final project\BTC CSV read.py"