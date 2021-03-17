# OBS 的一些设置

## 1. 录像设置

### 1.1. NVIDIA新编码器设置(推荐)

```
编码器：NVENC H.264 (new)
速率控制：VBR
比特率：20000kbps
最大比特率：60000kbps
【速率控制：CQP 如果有VBR兼容问题，建议用此设置】
【CQ 级别：15 越小质量越好】
关键帧间隔(秒，0=自动)：2
预设 Preset：最高质量 或 质量
配置 Profile：high
向前考虑 Look-ahead：勾选
心里视觉调整：勾选
最大 B 帧：设为 4。如果您取消选中“Look-ahead”（前向考虑）选项，请将 B 帧数量从 4 降至 2。
-----------
音频编码：AAC
音频比特率：320kbps
声道：2
采样率：48000
```

### 1.2. CPU编码器设置

```
编码器：x264
速率控制：VBR
比特率：20000kbps
CRF：23默认值
关键帧间隔(秒，0=自动)：2
CPU 使用预设：[Very Show] / Shower / Show / Medium
预设 Preset：最高质量 或 质量
配置 Profile：默认 Main，推荐 High （节约 10%码率）
微调 Tune：无
-----------
音频编码：AAC
音频比特率：320kbps
声道：2
采样率：48000
```

## 2. 直播串流设置

### 1.1. NVIDIA新编码器设置(推荐)

```
编码器：NVENC H.264 (new)
速率控制：CBR
比特率：6000kbps【视平台而定】
关键帧间隔(秒，0=自动)：2
预设 Preset：最高质量 或 质量
配置 Profile：high
向前考虑 Look-ahead：不勾选，最大B帧填2。勾选，最大B帧填4，占用CUDA。建议不勾选。
心里视觉调整：勾选
最大 B 帧：设为 4。如果您取消选中“Look-ahead”（前向考虑）选项，请将 B 帧数量从 4 降至 2。
-----------
音频编码：AAC
音频比特率：320kbps
声道：2
采样率：48000
```

### 1.2. CPU编码器设置

```
编码器：x264
速率控制：CBR
比特率：6000kbps【视平台而定】
关键帧间隔(秒，0=自动)：2
CPU 使用预设：fast（9900k 占用 45%），faster（8700k），veryfast（不能比这个更低）
配置 Profile：High
微调 Tune：无
-----------
音频编码：AAC
音频比特率：320kbps
声道：2
采样率：48000
```





**级别 Level**：4.2（1080@60），5.0（2k@30，1080@72），**5.1（4k@30，2k@51，1080@120）**，5.2（4k@60，2k@108，1080@172）

GOP：【间隔时间\*帧率：例如：B 站至少 10 秒 1 个则设置为 600 以下】

**软件**：MediaCoder

```
B站视频预设：
H.264，VBR，2次，基准5900Kbps，最高23000Kbps
Profile【High】，Level【5.1】，Preset【Very Show】
GOP【1~600】
```

## 3.其他

采集卡，OBS 中直接设置分辨率和帧率即可。Preset【Medium】

Voicemeeter 做声音分流，否则采集卡无声音。

```
h.264参数命令：
--profile high --level 51 --preset veryshow --bitrate 5900 --vbv-maxrate 23000 --keyint 600 --min-keyint 1
```

6000 码率，veryfast ，VBR

吃鸡，直播，NVENC （8400 和 1660Ti 都带不动，2060 可使用 NVENC）

怪物猎人，单机大作 D 加密，x264（9700k 带不动，需用 NVENC）

守望先锋，优化好的 fps 游戏，x264

#### 3.2. 码率

speedtest.net 进行测速。 推荐 6000 码率。

B 站：无码率限制

斗鱼：普通 3Mbps，签约 4Mbps，头牌主播 10Mbps

Twitch：6Mbps

5M 以下：720@60 或者 1080@30

5M 以上：1080@60

CBR：码率稳定，简单场景清晰，复杂场景模糊。【适合直播，流量稳定】

VBR：可变码率，简单场景低码率，复杂场景高码率。【适合视频，瞬间流量可能很大】【选择二次渲染】

#### 3.3. 速率控制

CBR：固定码率，全程码率恒定，文件大小可预期，编码压力小，主要用于直播

VBR：可变码率，按需分配；简单场景低码率，复杂场景高码率。2-pass 更加精准。

CRF：固定质量模式，CRF 值越低，视频质量越高，也是动态码率。

ABR：平均码率，相当于码率波动更小的 VBR。

CQP：固定量化参数，相当于低级的 CRF。

直播使用：CBR

有码率限制使用：VBR

无码率限优先使用：CRF

#### 3.4. 预设 CPU

直播使用：Very Fast 或 Fast （9600k 以上 cpu 可选 Fast）

做视频：Very Slow，Slower，Slow，Medium 都可以建议前面 2 个选项

#### 3.5. 上传视频的压缩

软件：MediaCoder

码率模式：VBR 2 次编码模式

视频码率：比限制少 100 即可，19900Kbps

最高码率：比限制少 1000 即可，59000Kbps

规格：High

级别：5.1

预设：Very Slow

优化：看视频内容，默认 Normal 也可以。

GOP：1~600

#### 3.6. 帧基础知识

I 帧，完整帧

P 帧，预测帧比 I 节约一半

B 帧，双向预测帧比 P 节约一半

#### 3.7. 显卡编码

NVENC new > Fast > Very Fast

分辨率：1080p@60

编码器：NVENC H.264 (new)

速率控制：CBR

比特率：6000Kbps

关键帧间隔（秒，0=自动）：2

预设：最高质量

配置 Profile：high

#### 3.8. CPU 编码

分辨率：1080p@60

编码器：x264

速率控制：CBR

比特率：6000Kbps

关键帧间隔（秒，0=自动）：2

预设：Very Fast 或 Fast(9600k 最高只能选到 Fast)

配置 Profile：high
