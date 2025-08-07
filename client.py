import socket
from PIL import Image
import io

# 假設這裡你已經有了要發送的 PIL 圖像對象：my_image

my_image = Image.open(r"C:\Users\User\Desktop\NCKU\master\DLIC\final\cat.jpg")

# 將 PIL 圖像保存到字節緩衝區
byte_stream = io.BytesIO()
my_image.save(byte_stream, format="JPEG") # 你可以選擇 JPEG, PNG 等格式
image_bytes = byte_stream.getvalue()

# 獲取圖像數據的長度
image_len = len(image_bytes)

# 連接到服務器
client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
client_socket.connect(("192.168.2.99", 8008))

# 先發送圖像數據的長度（4字節）
client_socket.sendall(image_len.to_bytes(4, 'big'))

# 然後發送圖像數據
client_socket.sendall(image_bytes)

client_socket.close()