# OBS 的一些设置

### 1. 制作视频

**编码器**：x264

**速率控制**：VBR，2 次

**目标比特率**：6Mbps

**最高比特率**：24Mbps

**文件格式**：mkv

**音频**：采样 44.1 或 48，比特率>192

**预设 Preset** ：**Very Show** / Shower / Show / Medium

**配置文件 Profile**：默认 Main，推荐**High**（节约 10%码率）

**级别 Level**：4.2（1080@60），5.0（2k@30，1080@72），**5.1（4k@30，2k@51，1080@120）**，5.2（4k@60，2k@108，1080@172）

GOP：【间隔时间\*帧率：例如：B 站至少 10 秒 1 个则设置为 600 以下】

**软件**：MediaCoder

```
B站视频预设：
H.264，VBR，2次，基准5900Kbps，最高23000Kbps
Profile【High】，Level【5.1】，Preset【Very Show】
GOP【1~600】
```

### 2. 直播推流

**编码器**：x264 或 NVENC `翼王测评无差别，其他测评x264画质更好`

**预设 Preset**：veryfast 或 fast

`质量` 或 `最好质量` 均可 `NVENC中的预设基本都无差别，质量时显卡占用为8%左右`

**速率控制**：CBR 固定比特率

**音频**：采样 44.1，比特率>=192

**预设 Preset**：fast（9900k 占用 45%），faster（8700k），veryfast（不能比这个更低） `直播中，画质差别细微，但此项资源占用最少，推荐veryfast`

```
B站直播预设：
H.264，CBR，码率6000Kbps
Profile【High】，Preset【fast】(CPU复杂场景占用小于80%)
```

```
B站直播预设：显卡（Intel>Nvidia>AMD）
Intel(QuickSync H.264) :Profile【High】，Preset【quality】，码率5000，30帧（某些可以60帧）
Nvidia(NVENC H.264) : Profile【High】，Preset【低延迟高质量】，Level【4.2】，开启Two-Pass，码率6000，60帧
AMD(AMF)
```

### 3.其他

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
