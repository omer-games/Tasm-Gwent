# TASM Gwent

![Gameplay Screenshot](https://github.com/omer-games/Tasm-Gwent/blob/main/Screenshot.png)

TASM Gwent is a fully custom card game inspired by **Gwent from The Witcher 3: Wild Hunt**, programmed entirely in **TASM assembly**.  
It’s a passion project that demonstrates low-level programming complexity while recreating the fun and strategy of Gwent in a DOS-style game.  

---

## 🎮 Game Overview

TASM Gwent is a 2-player turn-based strategy card game. The objective is to **win 2 out of 3 rounds** by playing cards that give you the highest score on the board.  

The game is built entirely in **TASM (Turbo Assembler)**, making it a unique and challenging low-level implementation.  

---

## 🕹️ Rules & Gameplay

### 🎯 Objective
Win **2 out of 3 rounds** by having more total points on your board than your opponent at the end of each round.

### 🔑 How to Play
- Players take turns placing **one card at a time**.
- A round ends when **both players pass**.
- The player with the **higher total card value** wins the round.
- Choose a card by pressing **0–9** (selecting cards from your hand).
- Press `-` to **pass your turn**.

### 🃏 Card Basics
- **Number**: Each card has a point value.
- **Symbol**: Determines where it can be placed:
  - ⚔ **Sword** – Front row  
  - 🏹 **Bow** – Middle row  
  - 🔥 **Fireball** – Back row  
- You can place **up to 10 cards per row**.

---

### 🌟 Special Cards
- 🐉 **Dragon Card** – Destroys the last enemy card placed in the same row.  
- 🕵 **Spy Card** – Place on opponent’s row. Helps them but draws you **2 extra cards**.  
- 🍷 **Wine Card** – Doubles the value of all cards in a row on **your side**. Doesn’t affect legendaries.  
- 👑 **Legendary Card** – Immune to weather effects.  

---

### 🌦️ Weather Cards
- ❄ **Freeze** – All Sword cards become 1 point (except legendaries).  
- 🌧️ **Rain** – All Bow cards become 1 point (except legendaries).  
- ⚡ **Lightning** – All Fireball cards become 1 point (except legendaries).  
- ☀ **Sunshine** – Removes all weather effects.  

---

## ⚙️ How to Build & Run

1. **Clone the Repository:**
   ```bash
   git clone https://github.com/omer-games/TASM-Gwent.git
   ```

2. **Navigate to the Project Directory:**
   ```bash
   cd TASM/bin
   ```

3. **Assemble and Run the Game:**  
   Make sure you have **Turbo Assembler (TASM)** and **Turbo Linker (TLINK)** set up in DOSBox or a compatible environment.

   ```bash
   tasm base.asm
   tlink base.obj
   base.exe
   ```

---

## 🖥️ System Requirements
- DOSBox or real DOS environment  
- Turbo Assembler (TASM) & Turbo Linker (TLINK)  
- Minimal specs:  
  - CPU: Any x86 processor  
  - RAM: 4 MB+  
  - Storage: <1 MB  
  - Display: VGA-compatible  

---

## 👤 Developer
Made with ❤️ by **Omer-Games**  
