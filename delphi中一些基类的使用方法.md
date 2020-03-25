# TObject

```pascal

```

# TPersistent

```pascal
  TPersistent = class(TObject)
  private
    procedure AssignError(Source: TPersistent);
  protected
    // AssignTo 方法是一个虚方法，这个方法是从这个类继承的类必须要实现的一个方法。这个方法是直接抛出了一个异常
    // 因此如果子类要用该方法，那么就必须将该方法重写。因为只有被复制的对象才知道要复制那些内容。
    procedure AssignTo(Dest: TPersistent); virtual;
    procedure DefineProperties(Filer: TFiler); virtual;
    function GetOwner: TPersistent; dynamic;
  public
    destructor Destroy; override;
 	// Assign方法是一个虚方法，如果子类没有重写该方法，那么就会调用源对象的AssignTo方法。
    procedure Assign(Source: TPersistent); virtual;
    function GetNamePath: string; dynamic;
  end;
  
```

