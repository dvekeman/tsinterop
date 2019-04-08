declare namespace isc {
  /**
   * Maintains a representation of selection over a 2-dimensional grid of objects.
   *  Automatically created to manage cell-selection on CubeGrid widgets.
   */
  class CellSelection extends Class {
    /**
     * Creates a new CellSelection
     *
     * @param typeCheckedProperties {Partial<CellSelection>=} Object literal containing init properties that will be checked against the class field types.
     * @param uncheckedProperties {Object} Object literal containing init properties that will not be checked against the class field types.
     * @return {CellSelection} The new instance
     */
    static create(typeCheckedProperties?: Partial<CellSelection>, uncheckedProperties?: Object): CellSelection;


    /**
     *  Return true if a particular item is selected
     *
     * @param {Integer} row index of the cell to check
     * @param {Integer} column index of the cell to check
     */
    cellIsSelected(rowNum: Integer, colNum: Integer): boolean;

    /**
     *  Is anything in the list selected?
     *
     */
    anySelected(): boolean;

    /**
     *  Returns an array of the currently selected cells. Each cell is returned as a 2 element
     *  array in the form [rowNum, colNum].
     *
     */
    getSelectedCells(): Array<any>;

    /**
     *  Returns the first record that has any cells selected.
     *
     */
    getSelectedRecord(): ListGridRecord;

    /**
     *  Observable handler fired whenever the cell selection is modified
     *
     *
     */
    selectionChanged(): void;

    /**
     *  Select a particular cell
     *
     *
     * @param {Integer} row index of cell to select
     * @param {Integer} column index of cell to select
     */
    selectCell(rowNum: Integer, colNum: Integer): boolean;

    /**
     *  Deselect a particular cell
     *
     *
     * @param {Integer} row index of the cell to select
     * @param {Integer} column index of the cell to select
     */
    deselectCell(rowNum: Integer, colNum: Integer): boolean;

    /**
     *   select an array of cells
     *
     * @param {Array<Partial<any>>} Array of cells to select. Each cell can be specified                    as a 2 element array [rowNum, colNum]
     */
    selectCellList(list: Array<Partial<any>>): boolean;

    /**
     *   deselect an array of cells
     *
     *
     * @param {Array<Partial<any>>} Array of cells to deselect. Each cell can be specified                    as a 2 element array [rowNum, colNum]
     */
    deselectCellList(list: Array<Partial<any>>): boolean;

    /**
     *   select a single cell and deselect everything else
     *
     * @param {Integer} row index of cell to select
     * @param {Integer} column index of cell to select
     */
    selectSingleCell(rowNum: Integer, colNum: Integer): boolean;


  }

  /**
   * A whole number, for example, 5. Decimal numbers, for example 5.5, are not allowed. Null is
   *  allowed.
   */
  type Integer = number;

}