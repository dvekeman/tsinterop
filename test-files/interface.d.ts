declare namespace isc {
  /**
   * Comment
   */
  interface Palette {
    /**
     * var comment
     */
    defaultEditContext?: EditContext | EditTree | EditPane;

    generateNames?: boolean;

    setDefaultEditContext?(defaultEditContext: EditContext | EditTree | EditPane): void;

    makeEditNode?(paletteNode: PaletteNode): EditNode;

    foo(bar: string): number;

  }

  class ListGrid implements Palette {
    generateNames: boolean;
    makeEditNode(paletteNode: PaletteNode): EditNode;
    foo(): number;
  }

}