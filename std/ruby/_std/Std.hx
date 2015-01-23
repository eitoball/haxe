package;

@:keepInit
@:coreApi class Std {
  public static inline function instance<T:(), S:T>( value : T, c : Class<S> ) : S
  {
    return null;
  }

  public static function string( s : Dynamic ) : String
  {
    return "";
  }

  public static function is( v : Dynamic, t : Dynamic ) : Bool
  {
    return false;
  }

  public static function parseInt( x : String ) : Null<Int>
  {
    return null;
  }

  public static function parseFloat( x : String ) : Float
  {
    return Math.NaN;
  }

  public static inline function random( x : Int ) : Int
  {
    return 0;
  }

  public static inline function int( x : Float ) : Int
  {
    return null;
  }
}
