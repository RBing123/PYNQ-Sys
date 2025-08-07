This repo is about PYNQ.  
Weight and bias are available in [HERE(BaiDu CloudDisk) URL](https://pan.baidu.com/s/1pconJnUbu52KN4jd-1EgKg)  
System Overview:
![image1](https://github.com/RBing123/PYNQ-Sys/blob/main/method.png)
Continue with the step of creating yolov2.tcl and yolov2.bit
1. Copy yolov2.tcl and yolov2.bit to this repo
2. Open yolov2.ipynb in jupyter notebook
3. You can change the image path:  
```python
#image path
ORIG_IMG_PATH = 'dog.jpg'
```
4. You can also change the bitstream:(The name of .tcl must be same as the name of .bit)  
```python
overlay = Overlay("yolov2.bit")
```
5. Demo video
[Link](https://youtu.be/4zyFfL_v7Vc)
