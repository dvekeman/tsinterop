declare namespace isc {
  interface Palette {
    makeEditNode?(paletteNode: PaletteNode): EditNode;
  }

  class ListGrid implements Palette {
    makeEditNode(paletteNode: PaletteNode): EditNode;
  }

}