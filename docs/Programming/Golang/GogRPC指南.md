# Go gRPC 指南

## 1. 准备工作

### 1.1. 安装 Protocol Buffers

https://developers.google.com/protocol-buffers/

直接下载对应二进制版本即可。将`protoc`放到 Path 目录中。

### 1.2. 安装下面的包

```bash
go get -u github.com/golang/protobuf/proto
go get -u github.com/golang/protobuf/protoc-gen-go
go get -u google.golang.org/grpc
go get -u golang.org/x/net/context
```

## 2. 实现

### 2.1. Proto

创建文件`protos/helloworld.proto`

```protobuf
syntax = "proto3";

package protos;

// 定义一个服务
service Greeter {
  // 定义一个函数
  rpc SayHello (HelloRequest) returns (HelloReply) {}
}

// 定义函数的输入
message HelloRequest {
  string name = 1;
}

// 定义函数的输出
message HelloReply {
  string message = 1;
}
```

到 `protos` 目录运行命令：

```bash
protoc --go_out=plugins=grpc:. *.proto
```

即可生成 `protos/helloworld.pb.go` 文件

### 2.2. Server

```go
package main

import (
	"log"
	"net"

	"golang.org/x/net/context"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	pb "github.com/jasperxu/go/demo/server/protos"
)

const (
	port = ":50051"
)

// GreeterServerImplementation is used to implement helloworld.GreeterServer.
type GreeterServerImplementation struct{}

// SayHello implements helloworld.GreeterServer
func (s *GreeterServerImplementation) SayHello(ctx context.Context, in *pb.HelloRequest) (*pb.HelloReply, error) {
	return &pb.HelloReply{Message: "Hello " + in.Name}, nil
}

func main() {
	lis, err := net.Listen("tcp", port)
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}
	s := grpc.NewServer()
	pb.RegisterGreeterServer(s, &GreeterServerImplementation{})
	// Register reflection service on gRPC server.
	reflection.Register(s)
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}

```

### 2.3. Client

```go
package main

import (
	"log"
	"os"

	"golang.org/x/net/context"
	"google.golang.org/grpc"

	pb "github.com/jasperxu/go/demo/server/protos"
)

const (
	address     = "localhost:50051"
	defaultName = "world"
)

func main() {
	// Set up a connection to the server.
	conn, err := grpc.Dial(address, grpc.WithInsecure())
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}
	defer conn.Close()
	c := pb.NewGreeterClient(conn)

	// Contact the server and print out its response.
	name := defaultName
	if len(os.Args) > 1 {
		name = os.Args[1]
	}
	r, err := c.SayHello(context.Background(), &pb.HelloRequest{Name: name})
	if err != nil {
		log.Fatalf("could not greet: %v", err)
	}
	log.Printf("Greeting: %s", r.Message)
}
```

## 3. 其他

### 3.1. Proto 指南

中文：http://blog.csdn.net/u011518120/article/details/54604615#PackagesandNameResolution

原版：https://developers.google.com/protocol-buffers/docs/overview?hl=zh-cn

go 语言：https://developers.google.com/protocol-buffers/docs/gotutorial?hl=zh-cn

API 部分：https://developers.google.com/protocol-buffers/docs/reference/overview?hl=zh-cn
