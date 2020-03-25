1. 更新环境
2. 编译代码
3. 提交代码

## 新建DLL工程要修改的配置

## Delphi Compiler

* 打开该工程的Option，修改该工程的 **Output directory** , **Search path** ，其中  **Search path** 的配置如图所示

![](新建dll1.png)

图片中的5条记录是必须添加的记录。

## Version Info

* 新建的Dll必须勾选 **Include version information in project**。不选中就那么编译出的Dll没有版本信息![](新建dll2.png)

## Packages

* 新的Dll接口只和PMDzpb这个包有关。![](新建dll3.png)

## Debugger

* Host application 要关联相应的应用程序![](新建dll4.png)

## 新建GUID

* 使用软件生成新的GUID。
* 新生成配置文件，以刚才生成的GUID命名。
* 修改对应路径下的Dzpb.Xml文件，将上面生成的配置文件路径添加到文件中。
* 配置模版信息。

{E9D94203-8998-4D90-B869-75BC79C48558}

{E9D94203-8998-4D90-B869-75BC79C48558}