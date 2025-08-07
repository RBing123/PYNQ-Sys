# image_server.py content

import socket
import threading
from PIL import Image as PIL_Image
import io
import time # Added for potential future use or debugging in server logic

# Global variables to store the received image and a lock for thread safety
received_image = None
image_lock = threading.Lock()

class Carame_Accept_Object:
    def __init__(self):
        self.host = '0.0.0.0'  # Listen on all available interfaces
        self.port = 8000       # Port number
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1) # Allow reuse of address
        try:
            self.server.bind((self.host, self.port))
            self.server.listen(5)
            print(f"[ImageServer] 服務器正在監聽 {self.host}:{self.port}")
        except Exception as e:
            print(f"[ImageServer] 服務器綁定或監聽失敗: {e}")
            self.server.close()
            exit() # Exit if server cannot start

    def run_server_loop(self):
        """This method contains the main server accept loop."""
        while True:
            try:
                client, D_addr = self.server.accept()
                print(f"[ImageServer] 客戶端連接: {D_addr}")
                clientThread = threading.Thread(target=RT_Image, args=(self, client, D_addr,), daemon=True) # daemon=True ensures thread exits with main program
                clientThread.start()
            except Exception as e:
                print(f"[ImageServer] 接受客戶端連接時發生錯誤: {e}")
                break # Exit loop on critical error

def RT_Image(camera_obj, client_socket, client_address):
    """Handles receiving image data from a connected client."""
    global received_image
    try:
        while True:
            # Receive image size (assuming 4 bytes)
            data_len_bytes = client_socket.recv(4)
            if not data_len_bytes:
                print(f"[ImageServer] 客戶端 {client_address} 斷開連接 (無數據長度)。")
                break
            data_len = int.from_bytes(data_len_bytes, 'big')

            # Receive image data
            img_data = b''
            bytes_received = 0
            while bytes_received < data_len:
                packet = client_socket.recv(min(data_len - bytes_received, 4096)) # Receive in chunks
                if not packet:
                    print(f"[ImageServer] 客戶端 {client_address} 斷開連接 (數據不完整)。")
                    break
                img_data += packet
                bytes_received += len(packet)

            if bytes_received == data_len:
                try:
                    # Convert bytes data to PIL Image
                    img_pil = PIL_Image.open(io.BytesIO(img_data))
                    
                    with image_lock:
                        received_image = img_pil
                    print(f"[ImageServer] 收到來自 {client_address} 的圖像，大小: {img_pil.size}")
                except Exception as img_e:
                    print(f"[ImageServer] 無法解碼來自 {client_address} 的圖像數據或格式錯誤: {img_e}")
            else:
                print(f"[ImageServer] 接收到的數據長度不匹配，預期: {data_len}, 實際: {bytes_received}")
                break

    except Exception as e:
        print(f"[ImageServer] 處理客戶端 {client_address} 時發生錯誤: {e}")
    finally:
        client_socket.close()
        print(f"[ImageServer] 客戶端 {client_address} 連接關閉。")

# This block allows running the server directly from this .py file if needed
if __name__ == '__main__':
    server_instance = Carame_Accept_Object()
    server_instance.run_server_loop()