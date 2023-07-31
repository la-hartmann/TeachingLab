from monday import MondayClient

monday = MondayClient('your token')

monday.items.create_item(board_id='2208860812', group_id='today',  item_name='Score1', column_values = facScore1)
