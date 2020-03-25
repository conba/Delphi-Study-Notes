1. 一个对象是什么，一个对象就是一个指针，这个指针指向该对象在内存中占据的一块空间。

2. 对象指针指向的内存空间成为对象空间。对象空间的头4个字节是指向该**对象直属类**的虚方法地址表 *VMT* ，接下来的空间就是存储对象本身成员数据的空间。

3. 每一个类都有对象的一张 *VMT* ，类的 *VMT* 保存从该类的原始祖先类派生到该类的所有的虚方法的过程地址。

4. *DELPHI* 的一个类，代表着一项 *VMT* 数据。

   ```pascal
   TClass = class of TObject;
   ```

   *TClass* 是类的类型，即，类之类。因此，类之类可以认为是为 *VMT* 数据项定义的类型，其实，他就是一个指向 *VMT* 数据的指针类型。

   正是由于 *DELPHI* 在应用程序中保留了完整的类信息，才能提供诸如 *as* 和 *is* 等在运行时刻转换和判别的高级面向对象功能，而类的 *VMT* 数据在其中起了关键性作用。

   有了类的类型，就可以将类作为变量来使用，可以将类的变量理解成一种特殊的对象。

   ```pascal
   TSampleClass = class of TSampleObject;
   TSampleObject = class(TObject)
   public
     constructor Create;
     destructor Destroy; override;
     class function GetSampleObjectCount: integer;
     procedure GetObjectIndex: integer;
   end;
   
   var
     aSampleClass: TSampleClass;
     aClass: TClass;
   ```

   这里可以将 *TSampleObject* 和 *TObject* 当作常量，并可以将他们赋值给 *aClass* 变量。就像将 *123* 常量赋值给变量*i*一样。所以，类类型，类和类变量的关系 就像 类型，常量和变量的关系。只不过是在类的层次上而不是对象的层次上。当然，直接将 *TObject* 赋值给 *aSampleClass* 是不合法的，因为 *aSampleClass* 是 *TObject* 派生类 *TSampleClass* 的类变量，而 *TObject* 并不包含与 *TSampleClass* 类型类型兼容的所有定义。相反，将 *TSampleObject* 赋值给 *aClass* 变量却是合法的，因为 *TSampleObject* 是 *TObject* 的派生类，是和 *TClass* 类型兼容的。这与对象变量的赋值和类型匹配关系是完全相似的。

5. 类方法，就是指在类的层次上调用的方法。在 *TObject* 中有大量的类方法，例如 *ClassName* 、 *ClassInfo* 和 *NewInstance* 等。其中 *NewInstance* 还被定义为虚方法，即虚的类方法。

6. 在类方法中可以使用 *self* 这个标识符，不过其所代表的含义与对象中的 *self* 是不同的。类方法中的 *self* 表示的是自身类，即指向 *VMT* 的指针，而对象方法中的 *self* 表示的是对象本身，即指向对象数据空间的指针。**虽然类方法只能在类层次上使用，但你仍可以通过一个对象区调用类方法**。例如，通过语句 *aObject*.*ClassName* 调用对象 *TObject* 的类方法 *ClassName* ，
