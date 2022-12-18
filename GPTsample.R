# 創建迷宮
maze <- matrix(c(1,1,1,1,1,
                 1,0,0,0,1,
                 1,0,1,0,1,
                 1,0,0,0,1,
                 1,1,1,1,1), nrow = 5, ncol = 5, byrow = TRUE)

# 初始化玩家位置
player_pos <- c(2,2)

# 迷宮遊戲主循環
while(TRUE) {
  # 打印迷宮和玩家位置
  maze[player_pos[1], player_pos[2]] <- 2
  print(maze)
  maze[player_pos[1], player_pos[2]] <- 0
  
  # 讀取玩家輸入并更新玩家位置
  move <- readline(prompt = "請輸入移動方向 (w/a/s/d): ")
  if (move == "w") {
    player_pos[1] <- player_pos[1] - 1
  } else if (move == "a") {
    player_pos[2] <- player_pos[2] - 1
  } else if (move == "s") {
    player_pos[1] <- player_pos[1] + 1
  } else if (move == "d") {
    player_pos[2] <- player_pos[2] + 1
  }
  
  # 判斷玩家是否到達終點
  if (maze[player_pos[1], player_pos[2]] == 1) {
    print("你撞墻了!")
  } else if (player_pos[1] == 1 && player_pos[2] == 4) {
    print("恭喜你成功到達終點!")
    break
  }
}
