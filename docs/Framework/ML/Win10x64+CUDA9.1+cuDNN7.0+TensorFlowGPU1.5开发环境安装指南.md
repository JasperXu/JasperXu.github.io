# Win10 x64 + CUDA 10.0 + cuDNN v7.5 + TensorFlow GPU 1.13 安装指南

Update : 2019.03.08

## 0. 环境说明

硬件：Ryzen R7 1700x + GTX 1080Ti

系统：Windows 10 Enterprise Version 1809 Update March 2019

## 1. 前期工作

NVIDIA 419.35 驱动

Visual Studio 2017 （需要 C++部分）

Python 3.6.x x64

## 2. 安装 CUDA 和 cuDNN

### 2.1. CUDA 10.0

下载地址：https://developer.nvidia.com/cuda-10.0-download-archive

### 2.2. cuDNN v7.5 for CUDA 10.0

下载地址：https://developer.nvidia.com/rdp/cudnn-download

解压后覆盖到`C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v10.0`目录即可。

### 2.3. CUDA Profiler Tools Interface (CUPTI)[可选]

下载地址：https://developer.nvidia.com/CUPTI

CUDA 工具包附带

### 2.4. NCLL 2.4[可选]

下载地址：https://developer.nvidia.com/nccl/nccl-download

可实现多 GPU 支持。仅支持 Linux。

### 2.5. TensorRT 5.0[可选]

下载地址：https://developer.nvidia.com/nvidia-tensorrt-download

可缩短在某些模型上进行推断的延迟并提高吞吐量。仅支持 Linux。

## 3. 安装 DXSDK_Jun10【不确定是否必须】

DXSDK_Jun10.exe 下载地址：https://www.microsoft.com/en-us/download/details.aspx?id=6812

Win10 安装时会提示 S1023 的错误，不用管。`C:\Program Files (x86)\Microsoft DirectX SDK (June 2010)\Include\d3dx9.h` ，`d3dx10.h` ，`d3dx11.h` 文件存在就可以。

这些都装完了就可以到`C:\ProgramData\NVIDIA Corporation\CUDA Samples\v8.0`中打开项目。

一共 155 个，都编译成功即可。然后到`C:\ProgramData\NVIDIA Corporation\CUDA Samples\v8.0\bin\win64\`下的`Debug`或`Release`内找到`deviceQuery.exe`，用命令行运行，不报错说明 CUDA 安装成功。

## 4. 安装 Tensorflow GPU 1.13.1

```bash
pip install --upgrade tensorflow-gpu
```

使用下列代码测试安装正确性

```python
>>> import tensorflow as tf
>>> hello = tf.constant('Hello, TensorFlow!')
>>> sess = tf.Session()
>>> print(sess.run(hello))
```

## 5. 常用库

```bash
pip install --upgrade numpy
pip install --upgrade scipy
pip install --upgrade pandas
pip install --upgrade keras
pip install --upgrade matplotlib
```

## 6. 其他

Ryzen R7 1700x 支持：SSE、SSE2、SSE4.1、SSE4.2、AVX、AVX2、FMA。

Win10 下的`nvidia-smi`在 `C:\Program Files\NVIDIA Corporation\NVSMI` 目录内。
